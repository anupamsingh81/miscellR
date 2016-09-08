

library(shiny)

shinyUI(pageWithSidebar(
  
  
  headerPanel("Comparing Paired Samples"),
  
  
  sidebarPanel(
    
    p('Input values can be separated by', br(),
      'newlines, spaces, commas, or tabs.'),
    
    p(strong("Data 1 (e.g., pretest):")),
    tags$textarea(id="data1", rows=20, cols=10, "70\n50\n80\n70\n25\n39\n40\n39\n90\n77\n39\n66\n70\n89\n69\n88\n49\n52\n36\n56\n67\n57"),
    
    p(br()),
    
    p(strong("Data 2 (e.g., posttest):")),
    tags$textarea(id="data2", rows=20, cols=10, "69\n59\n80\n99\n40\n72\n39\n37\n90\n87\n76\n60\n55\n78\n72\n92\n55\n47\n66\n77\n65\n50")
    
  ),
  
  
  mainPanel(
    tabsetPanel(
      
      tabPanel("Main",
               
               h3("Basic statistics"),
               verbatimTextOutput("textarea.out"),
               
               br(),
               
               h3("Overlayed histograms"),
               
               plotOutput("distPlot"),
               
               h3("Box plots with individual data points"),
               
               plotOutput("boxPlot", width="80%"),
               
               br(),
               
               h3("Changes of the individual data"),
               
               plotOutput("indvPlot", width="70%"),
               
               br(),
               
               h3("Scatterplot"),
               
               plotOutput("correlPlot", width="70%", height="500px"),
               
               br(),
               
               h3("Test of normality (Each data)"),
               verbatimTextOutput("testnorm.out"),
               
               br(),
               
               h3("Distribution of the difference scores"),
               
               plotOutput("distdiffPlot", width="70%"),
               
               br(),
               
               h3("Checking the normality of difference scores"),
               
               verbatimTextOutput("diffnorm.out"),
               
               br(),
               
               h3("Paired t-test"),
               verbatimTextOutput("t.out"),
               
               br(),
               
               h3("Effect size indices"),
               verbatimTextOutput("es.out"),
               
               br(),
               
               h3("Wilcoxon signed-rank test"),
               verbatimTextOutput("wsr.out"),
               
               br(),
               
               h3("Power analysis (Just for a reference)"),
               verbatimTextOutput("power.out"),
               
               br(),
               br(),
               
               strong('R session info'),
               verbatimTextOutput("info.out")
               
      ),
      
      tabPanel("About",
               
               strong('Note'),
               p('This web application is developed with',
                 a("Shiny.", href="http://www.rstudio.com/shiny/", target="_blank"),
                 ''),
               
               br(),
               
               strong('List of Packages Used'), br(),
               code('library(shiny)'),br(),
               code('library(psych)'),br(),
               code('library(car)'),br(),
               code('library(compute.es)'),br(),
               code('library(pwr)'),br(),
               code('library(lattice)'),br(),
               code('library(latticeExtra)'),br(),
               br(),
               
               h3("Overlayed histograms"),
               
               plotOutput("plot.out"),
               
               
               
               strong('Code'),
               p('Source code for this application is based on',
                 a('"The handbook of Research in Foreign Language Learning and Teaching" (Takeuchi & Mizumoto, 2012).', href='http://mizumot.com/handbook/', target="_blank")),
               
               p('The code for this web application is available at',
                 a('GitHub.', href='https://github.com/mizumot/paired', target="_blank")),
               
               p('If you want to run this code on your computer (in a local R session), run the code below:',
                 br(),
                 code('library(shiny)'),br(),
                 code('runGitHub("paired","mizumot")')
               ),
               
               br(),
               
               strong('Recommended'),
               p('To learn more about R, I suggest this excellent and free e-book (pdf),',
                 a("A Guide to Doing Statistics in Second Language Research Using R,", href="http://cw.routledge.com/textbooks/9780805861853/guide-to-R.asp", target="_blank"),
                 'written by Dr. Jenifer Larson-Hall.'),
               
               p('Also, if you are a cool Mac user and want to use R with GUI,',
                 a("MacR", href="http://www.urano-ken.com/blog/2013/02/25/installing-and-using-macr/", target="_blank"),
                 'is defenitely the way to go!'),
               
               br(),
               
               strong('Author'),
               p(a("Atsushi MIZUMOTO,", href="http://mizumot.com", target="_blank"),' Ph.D.',br(),
                 'Associate Professor of Applied Linguistics',br(),
                 'Faculty of Foreign Language Studies /',br(),
                 'Graduate School of Foreign Language Education and Research,',br(),
                 'Kansai University, Osaka, Japan'),
               
               br(),
               
               a(img(src="http://i.creativecommons.org/p/mark/1.0/80x15.png"), target="_blank", href="http://creativecommons.org/publicdomain/mark/1.0/")
      )
    )
  )
))


