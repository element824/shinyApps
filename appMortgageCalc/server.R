# load libraries
library(shiny)
library(ggplot2)
library(plyr)
library(reshape2)

# function to calculate monthly payments
paymnth=function(loanamt,irate,nyr,xtraprin,xtraprintime){
  
  # Calculate monthly payments
  
  # loanamt - loan amount
  # irate -  interest rate
  # nyr number of years of mortgage
  # xtraprin extra amount paid towards principal per period
  # xtraprintime period frequency when extra principal is paid
  
  # NPV factor  
  disc=1/(1+(irate/12))
  discfact=(1-disc^(nyr*12))/(1-disc)
  
  # monthly payment (principal + interest)
  mnthpay=loanamt/discfact*(1/disc)
  
  # Monthly split of interest and principal
  nmnth=nyr*12
  
  imnth=rep(0,nmnth) # interest paid each month
  pmnth=rep(0,nmnth) # principal paid each month
  resloan=rep(0,nmnth) # remaining principal after payment in a month
  
  for(i in 1:nmnth){
    
    if(i==1){
      
      imnth[i]=loanamt*(irate/12)
      pmnth[i]=mnthpay-imnth[i]
      resloan[i]=loanamt-pmnth[i]
      
    } else if (resloan[i-1]>0) {
      
      imnth[i]=resloan[i-1]*(irate/12)
      if((i-1) %% xtraprintime == 0){
        pmnth[i]=mnthpay-imnth[i]+xtraprin
      } else {
        pmnth[i]=mnthpay-imnth[i]
      }
      pmnth[i]=min(pmnth[i],resloan[i-1])
      resloan[i]=resloan[i-1]-pmnth[i]
      
    }
    
  }
  
  
  df=data.frame(imnth=imnth,pmnth=pmnth,resloan=resloan)
  df$mnth=seq(1:nmnth)
  df$yr=rep(c(1:nyr),each=12)
  df$totmnth=df$imnth+df$pmnth
  df$cum_imnth=cumsum(df$imnth)
  df$cum_pmnth=cumsum(df$pmnth)
  
  return(df)
  
} # end paymnth function



shinyServer(function(input, output) {
  
  data=reactive({
    
    ## home price, downpayment and mortgage rates
    homeprice=input$homeprice # home price
    pctdown=input$pctdown/100 # percentage downpayment
    loanamt=homeprice*(1-pctdown) # loan amount
    nyr=input$nyr # loan time period in years
    irate=input$irate/100 # interest rate
    xtraprin=input$xtraprin # extra amt to principal
    xtraprintime=switch(input$xtraprintime,
                        "monthly"=1,
                        "quarterly"=3,
                        "half-yearly"=6,
                        "yearly"=12) # frequency of extra amt to principal 
    
    # property tax and home owner insurance
    ptaxrate=input$ptaxrate/100 # annual property tax
    ptaxyr=homeprice*ptaxrate

    
    # tax break
    dedother=input$dedother # other deductions in itemized deduction
    taxrate=input$taxrate/100 # tax rate
    dedstd=input$dedstd # standard deduction
    
    # 401K loan
    loan401K=input$loan401K # 401K loan amount
    irate401K=input$irate401K/100 # interest rate for 401K
    nyr401K=input$nyr401K # period for 401K
    
    
    pi_df=paymnth(loanamt,irate,nyr,xtraprin,xtraprintime)
    pi_df$ptax=homeprice*ptaxrate/12
    pi_df$hins=input$insamt # insurance amt per month
    pi_df$hoa=input$hoa/12  # HOA amount per month
    
    intyr=ddply(pi_df,c("yr"),summarize,intyr=sum(imnth))
    intyr$taxbreakmnth=pmax(intyr$intyr+ptaxyr+dedother-dedstd,0)*taxrate/12
    
    pi_df2=merge(pi_df,intyr[,c("yr","taxbreakmnth")],by=c("yr"))
    
    
    pay401K=paymnth(loan401K,irate401K,nyr401K,0,1)
    pay401K$totmnth401K=pay401K$totmnth
    
    pi_df3=merge(pi_df2,pay401K[,c("mnth","totmnth401K")],by=c("mnth"),all.x=TRUE)
    pi_df3$totmnth401K[is.na(pi_df3$totmnth401K)]=0
    pi_df3$totmnthfull=pi_df3$totmnth+pi_df3$ptax+pi_df3$hins+pi_df3$hoa-pi_df3$taxbreakmnth+pi_df3$totmnth401K
    
    pi_df3
    
  })
  
  datayrfilter=reactive({
    pi_df3=data()
    pi_df3$taxred=pi_df3$taxbreakmnth*(-1)
    pi_df3tall=melt(pi_df3,id.vars=c("mnth","yr"),measure.vars=c("imnth","pmnth","ptax","hins","hoa","taxred","totmnth401K"),
                    variable.name=c("varname"),value.name=c("value"))
    
    pi_df4tall=ddply(pi_df3tall,c("yr","varname"),summarize,value=mean(value))
    
    payyrpick=input$payyrpick
    pi_df4tallpickyr=pi_df4tall[pi_df4tall$yr == payyrpick,]
    pi_df4tallpickyr
  })
  
  output$paymnthPlot=renderPlot({
    nyr=input$nyr
    p=ggplot(data=data(),aes(x=mnth,y=totmnthfull))+geom_point()
    p=p+scale_x_continuous(breaks=seq(0,12*nyr,12),labels=seq(0,nyr))
    p=p+xlab("Year")+ylab("Monthly Payment ($)")+
      theme_bw(20)+
      theme(axis.text.x=element_text(angle=-90))
    print(p)
      })
  
  output$paymnthbreakout=renderPlot({
    tmpdf=datayrfilter()
    p=ggplot(data=tmpdf,aes(x=varname,y=value))+geom_bar(stat="identity",fill="lightblue")+
      geom_text(aes(label=round(value,0)),size=7,vjust=1)+
      scale_x_discrete(label=c("Interest","Principal","Property Tax","Insurance","HOA","Tax Break","401K loan"))+
      xlab("")+ylab("Monthly Payment ($)")+
      theme_bw(20)+
      theme(axis.text.x=element_text(angle=-90))
    print(p)
  })
  
  output$tblOut=renderTable({
    # show payment for a given set of years
    pi_df3=data()
    yrlist=c(1,2)
    pay3yr=pi_df3[pi_df3$yr %in% yrlist,
                  c("mnth","yr","totmnth","ptax","hins","hoa","taxbreakmnth","totmnth401K","totmnthfull")]
    names(pay3yr)=c("Month","Year","Principal and Interest",
                    "Property Tax","Home Insurance","HOA dues","Tax Break","401K Loan Payment","Total Monthly Payment")
    pay3yr
  })
  
  output$cashoutlay=renderText({
    cashout=input$homeprice*input$pctdown/100-input$loan401K
    sprintf("Initial Cash Outlay: $ %5.0f",cashout)
          })
  
  output$avgmnthpay=renderText({
    avgmnthpay=sum(datayrfilter()$value)
    sprintf("Avg monthly payment: $ %5.0f",avgmnthpay)
  })
  
})