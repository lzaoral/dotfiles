Config { font = "Terminus 10"
       , bgColor = "black"
       , fgColor = "#878787"
       , alpha = 255
       , position = Bottom
       , textOffset = 1
       , template = "}%StdinReader%{%multicpu% | %multicoretemp% | %memory% %swap% | %volume% | %battery% | %bright% | %locks% | %kbd% | %LKTB% | <fc=#ee9a00>%date%</fc> |%trayerpad%"
       , commands = [ Run Weather "LKTB" [
                               "-t", "Brno: <tempC>°C"
                             , "-L", "15", "-H", "25"
                             , "--normal", "green"
                             , "--high", "red"
                             , "--low", "lightblue"
                             ] 3600
                    , Run Memory ["-t","Mem: <usedratio>%"] 50
                    , Run Date "%a %_d %b %Y %H:%M:%S" "date" 10
                    , Run StdinReader
                    , Run MultiCpu [
                               "--template" , "Cpu: <total>%"
                             , "--Low"      , "50"         -- units: %
                             , "--High"     , "85"         -- units: %
                             , "--low"      , "darkgreen"
                             , "--normal"   , "darkorange"
                             , "--high"     , "darkred"
                             ] 10
                    , Run MultiCoreTemp [
                               "--template" , "Max: <max>°C Avg: <avg>°C"
                             , "--Low"      , "60"         -- units: °C
                             , "--High"     , "80"         -- units: °C
                             , "--low"      , "darkgreen"
                             , "--normal"   , "darkorange"
                             , "--high"     , "darkred"
                             , "--"         , "--mintemp", "20"
                                             , "--maxtemp", "100"
                             ] 50
                    , Run Battery [
                               "--template" , "Batt: <acstatus>"
                             , "--Low"      , "10"         -- units: %
                             , "--High"     , "50"         -- units: %
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
                    , Run Brightness
                       [ "--template", "Bright: <percent>%"
                       , "--", "-D", "intel_backlight"
                       ] 4
                    , Run Com "/home/lukas/.config/xmobar/volume.sh" [] "volume" 10
                    , Run Com "/home/lukas/.config/xmobar/padding-icon.sh" ["panel"] "trayerpad" 10
                    ]
}
