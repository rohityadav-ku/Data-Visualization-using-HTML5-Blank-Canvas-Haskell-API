{-# LANGUAGE OverloadedStrings #-}

import Graphics.Blank
import qualified Data.Text as Text
import Control.Applicative
import Data.Monoid

drawpiechart :: DeviceContext -> IO()
drawpiechart context = send context $ do
        combine $ iterate_tuple_list context dataseries1  

drawbarchart :: DeviceContext -> IO()
drawbarchart context = send context $ do
        combine $ iterate2 context dataseries2

drawcolumnchart:: DeviceContext -> IO()
drawcolumnchart context = send context $ do
        combine $ iterate3 context dataseries3

iterate_tuple_list :: DeviceContext -> [((Double,Double),Text.Text)] -> [Canvas()]
--iterate_tuple_list context data1 = piechart context $ head data1
iterate_tuple_list context data1 = map (piechart context) data1

iterate2 :: DeviceContext -> [(Text.Text,Double,Double,Double,Double,Double,Double,Text.Text,Text.Text,Double,Double)] -> [Canvas()]
iterate2 context data2 = map (barchart context) data2

iterate3 :: DeviceContext -> [(Text.Text,Double,Double,Double,Double,Double,Double,Text.Text,Double,Text.Text,Double,Double)] -> [Canvas()]
iterate3 context data3 = map (columchart context) data3

combine :: [Canvas ()] -> Canvas()
combine canvasList = foldl mappend mempty canvasList

piechart :: DeviceContext -> ((Double,Double),Text.Text) -> Canvas()
piechart context ((startangle,endangle),color) = do
        let xcentre = width context / 2;
        let ycentre = height context / 2;
        let radius = 120;
        beginPath()
        moveTo(xcentre,ycentre)
        arc(xcentre,ycentre,radius,startangle,endangle,False)
        strokeStyle color;
        stroke()
        fillStyle color;
        fill()
        
barchart :: DeviceContext -> (Text.Text,Double,Double,Double,Double,Double,Double,Text.Text,Text.Text,Double,Double) -> Canvas()
barchart context (year,x_year,y_year,x1,y1,x2,y2,color,typ,x_end,y_end) = do
        textBaseline "middle";
        font "bold 20px sans-serif"; 
        fillText ("Company Performance" , 400 , 20)
        
        beginPath()
        moveTo(125,40)
        lineTo(125,600)
        strokeStyle "black";
        lineWidth 1
        stroke()

        beginPath()
        moveTo (125,600)
        lineTo (1000,600)
        strokeStyle "black";
        lineWidth 1
        stroke()

        beginPath()
        font "bold 15px sans-serif";
        fillText(year,x_year,y_year)
        moveTo(x1,y1)
        lineTo (x2,y2)
        lineWidth 30
        strokeStyle color;
        stroke()
        fillText(typ,x_end,y_end)

columchart :: DeviceContext -> (Text.Text,Double,Double,Double,Double,Double,Double,Text.Text,Double,Text.Text,Double,Double)-> Canvas ()
columchart context (val1,x_loc,y_loc,x1,y1,x2,y2,color,widt,typ,end1,end2) = do
            beginPath()
            font "bold 20px sans-serif"; 
            fillText ("Company Performance" , 500 , 100)
            font "bold 15px sans-serif";
            fillText(val1,x_loc,y_loc)
            moveTo(x1,y1)
            lineTo(x2,y2)
            strokeStyle color;
            lineWidth widt
            fillText(typ,end1,end2)
            stroke()


colors :: [Text.Text]
colors = ["#0000FF","#FF4500","#FFA500","#228B22","#800080","#FF0000"]

piechartPercentage :: [Double]
piechartPercentage = [60,30,90,100,80]

convert_to_radians :: Double -> [Double] -> [(Double,Double)]
convert_to_radians _ [] =[]
convert_to_radians n piechartPercentage = (n,sumPercentage) : convert_to_radians sumPercentage (tail piechartPercentage)
                                          where sumPercentage = sum [n,(head piechartPercentage / 180) * pi]

dataseries1 :: [((Double,Double),Text.Text)]
dataseries1 = zip (convert_to_radians 0 piechartPercentage) colors


--dataseries1 :: [(Double,Double,Text.Text)]
--dataseries1  = [(0,((60 / 180) * pi),"#0000FF"),
                --(((60 / 180) * pi),sum [((60 / 180) * pi),((30 / 180) * pi)],"#FF4500"),
                --(sum [((60 / 180) * pi),((30 / 180) * pi)],sum [((90 / 180) * pi),((40 / 180) * pi)],"#FFA500"),
                --(sum [((90 / 180) * pi),((40 / 180) * pi)],sum [((130 / 180) * pi),((70 / 180) * pi)],"#228B22"),
                --(sum [((130 / 180) * pi),((70 / 180) * pi)],sum [((200 / 180) * pi),((60 / 180) * pi)],"#800080"),
                --(sum [((200 / 180) * pi),((60 / 180) * pi)],2 * pi,"#FF0000")]   

 
dataseries2 :: [(Text.Text,Double,Double,Double,Double,Double,Double,Text.Text,Text.Text,Double,Double)]
dataseries2 = [("2010",80,120,125,100,600,100,"blue","Sales",650,100),
               ("",0,0,125,135,300,135,"red","Expenses",350,135),
               ("2011",80,220,125,200,800,200,"blue","Sales",850,200),
               ("",0,0,125,235,400,235,"red","Expenses",450,235),
               ("2012",80,320,125,300,400,300,"blue","Sales",450,300),
               ("",0,0,125,335,650,335,"red","Expenses",700,335),
               ("2013",80,420,125,400,650,400,"blue","Sales",700,400),
               ("",0,0,125,435,500,435,"red","Expenses",550,435),
               ("2014",80,520,125,500,600,500,"blue","Sales",650,500),
               ("",0,0,125,535,630,535,"red","Expenses",680,535)]

dataseries3 :: [(Text.Text,Double,Double,Double,Double,Double,Double,Text.Text,Double,Text.Text,Double,Double)]
dataseries3 =[("100",200,600,250,600,900,600,"black",1,"",0,0),
              ("400",200,525,250,525,900,525,"black",1,"",0,0),
              ("700",200,450,250,450,900,450,"black",1,"",0,0),
              ("1000",200,375,250,375,900,375,"black",1,"",0,0),
              ("1300",200,275,250,275,900,275,"black",1,"",0,0),
              ("2011",310,620,300,600,300,350,"blue",50,"Sales",280,330),
              ("",0,0,355,600,355,500,"red",50,"Expenses",335,480),
              ("2012",460,620,450,600,450,250,"blue",50,"Sales",430,230),
              ("",0,0,505,600,505,450,"red",50,"Expenses",485,430),
              ("2013",610,620,600,600,600,400,"blue",50,"Sales",580,380),
              ("",0,0,655,600,655,300,"red",50,"Expenses",635,280),
              ("2014",760,620,750,600,750,325,"blue",50,"Sales",730,305),
              ("",0,0,805,600,805,425,"red",50,"Expenses",785,405)]


main = do 
        putStrLn "Enter 1 to draw a PieChart"
        putStrLn "Enter 2 to draw a BarChart"
        putStrLn "Enter 3 to draw a ColumnChart"
        choice <- getLine
        case choice of
                "1" -> blankCanvas 3000 drawpiechart
                "2" -> blankCanvas 3000 drawbarchart
                "3" -> blankCanvas 3000 drawcolumnchart

