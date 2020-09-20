Config { font = "xft: xos4 Terminus:size=8"
       , borderColor = "black"
       , border = TopB
       , bgColor = "black"
       , fgColor = "grey"
       , alpha = 255
       , position = BottomP 0 0
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %multicpu% | %coretemp% | %memory% %swap% | %volume% | %battery% | %locks% | %kbd% | %LKTB% | %mpris2% | <fc=#ee9a00>%date%</fc>"
       , commands = [ Run Weather "LKTB" [
                               "-t", "Brno: <tempC>°C"
                             , "-L", "15", "-H", "25"
                             , "--normal", "green"
                             , "--high", "red"
                             , "--low", "lightblue"
                             ] 36000
                    , Run Memory ["-t","Mem: <usedratio>%"] 50
                    , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
                    , Run StdinReader
                    , Run MultiCpu [
                               "--template" , "Cpu: <total0>% <total1>% <total2>% <total3>% <total4>% <total5>% <total6>% <total7>%"
                             , "--Low"      , "50"         -- units: %
                             , "--High"     , "85"         -- units: %
                             , "--low"      , "darkgreen"
                             , "--normal"   , "darkorange"
                             , "--high"     , "darkred"
                             ] 10
                    , Run CoreTemp [
                               "--template" , "Temp: <core0>°C <core1>°C <core2>°C <core3>°C"
                             , "--Low"      , "70"        -- units: °C
                             , "--High"     , "80"        -- units: °C
                             , "--low"      , "darkgreen"
                             , "--normal"   , "darkorange"
                             , "--high"     , "darkred"
                             ] 50
                    , Run Battery [
                               "--template" , "Batt: <acstatus>"
                             , "--Low"      , "10"        -- units: %
                             , "--High"     , "50"        -- units: %
                             , "--low"      , "darkred"
                             , "--normal"   , "darkorange"
                             , "--high"     , "darkgreen"
                             , "--" , "-o"  , "<left>% (<timeleft>)"
                                    , "-O"  , "<fc=#dAA520>Charging</fc>"
                                    , "-i"  , "<fc=#006000>Charged</fc>"
                             ] 50
                    , Run Locks
                    , Run Kbd [ ("us", "US"), ("cz(qwerty)", "CZ") ]
                    , Run Swap [] 50
                    , Run Com "/home/lukas/.config/xmobar/volume.sh" [] "volume" 1
                    , Run Mpris2 "spotify" [
                                "-t", "<artist> - <title>"
                            ] 10
                    ]
}

