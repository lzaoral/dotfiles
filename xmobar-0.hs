Config { font = "xft:xos4 Terminus:size=11:bold:antialias=true"
       , borderColor = "black"
       , border = TopB
       , bgColor = "black"
       , fgColor = "grey"
       , alpha = 255
       , position = BottomP 0 0
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %multicpu% %coretemp% | %memory% %swap% | %locks% | %kbd% | %LKTB% | <fc=#ee9a00>%date%</fc> |                 "
       , commands = [ Run Weather "LKTB" [
                               "-t", "Brno: <tempC>°C"
                             , "-L", "18", "-H", "25"
                             , "--normal", "green"
                             , "--high", "red"
                             , "--low", "lightblue"
                             ] 36000
                    , Run Memory ["-t","Mem: <usedratio>%"] 50
                    , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
                    , Run StdinReader
                    , Run MultiCpu [
                               "--template" , "Cpu: <total0>% <total1>%"
                             , "--Low"      , "50"         -- units: %
                             , "--High"     , "85"         -- units: %
                             , "--low"      , "darkgreen"
                             , "--normal"   , "darkorange"
                             , "--high"     , "darkred"
                             ] 10
                    , Run CoreTemp [
                               "--template" , "Temp: <core0>°C <core1>°C"
                             , "--Low"      , "70"        -- units: °C
                             , "--High"     , "80"        -- units: °C
                             , "--low"      , "darkgreen"
                             , "--normal"   , "darkorange"
                             , "--high"     , "darkred"
                             ] 50
                                                 , Run Locks
                    , Run Kbd [ ("us", "US"), ("cz(qwerty)", "CZ") ]
                    , Run Swap [] 50
                    ]
}

