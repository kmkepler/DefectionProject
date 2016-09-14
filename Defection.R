library(plyr)

f <- "%d/%m/%Y"
setClass('fDate')
setAs(from="character",
      to="fDate", 
      def=function(from) as.Date(from, format=f))

COMPLAINTS <- read.csv('http://ballings.co/hidden/aCRM/data/chapter6/complaints.txt',sep=';',
                       colClasses=c('character',
                                    'character',
                                    'factor',
                                    'fDate',
                                    'factor',
                                    'factor',
                                    'factor'))

CREDIT <- read.csv('http://ballings.co/hidden/aCRM/data/chapter6/credit.txt',sep=';',
                   colClasses=c('character',
                                'character',
                                'factor',
                                'fDate',
                                'factor',
                                'numeric',
                                'integer'))

CUSTOMERS <- read.csv('http://ballings.co/hidden/aCRM/data/chapter6/customers.txt',sep=';',
                      colClasses=c('character',
                                   'factor',
                                   'fDate',
                                   'factor',
                                   'factor',
                                   'character'))

DELIVERY <- read.csv('http://ballings.co/hidden/aCRM/data/chapter6/delivery.txt',sep=';',
                     colClasses=c('character',
                                  'character',
                                  'factor',
                                  'factor',
                                  'factor',
                                  'fDate',
                                  'fDate'))

FORMULA <- read.csv('http://ballings.co/hidden/aCRM/data/chapter6/formula.txt',sep=';',
                    colClasses=c('character',
                                 'factor',
                                 'factor',
                                 'numeric'))

SUBSCRIPTIONS <- read.csv('http://ballings.co/hidden/aCRM/data/chapter6/subscriptions.txt',sep=';',
                          colClasses=c('character',
                                       'character',
                                       'factor',
                                       'factor',
                                       'fDate',
                                       'fDate',
                                       'integer',
                                       'integer',
                                       'fDate',
                                       'factor',
                                       'factor',
                                       'fDate',
                                       'character',
                                       'numeric',
                                       'numeric',
                                       'numeric',
                                       'numeric',
                                       'numeric',
                                       'numeric',
                                       'numeric',
                                       'numeric'))
