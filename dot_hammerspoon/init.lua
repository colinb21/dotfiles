
hs.alert.show("Config loaded")
--hs.loadSpoon("WinWin")
--hs.loadSpoon("WindowHalfsAndThirds")

local cac = {"cmd", "alt", "ctrl"}

function bindKey(key, fn)
  hs.hotkey.bind(cac, key, fn)
end

bindKey('l', function() hs.grid.show() end)

require "org-capture-integration"

--bindKey('h', function() spoon.WinWin:stepResize('left') end)
--bindKey(';', function() spoon.WinWin:stepResize('right') end)

-- bindKey(';', function() spoon.WinWin:moveToScreen('left') end)
-- bindKey('\\', function() spoon.WinWin:moveToScreen('right') end)

-- bindKey('i', function() spoon.WinWin:moveAndResize('halfup') end)

-- bindKey('j', function() spoon.WinWin:moveAndResize('halfleft') end)
-- bindKey('k', function() spoon.WinWin:moveAndResize('center') end)
-- bindKey('l', function() spoon.WinWin:moveAndResize('halfright') end)

--bindKey('m', function() spoon.WinWin:moveAndResize('cornerSW') end)
-- bindKey(',', function() spoon.WinWin:moveAndResize('halfdown') end)

-- bindKey('f', function() hs.window.focusedWindow():toggleFullScreen() end)

-- hs.loadSpoon("KSheet")
-- bindKey('c', function() spoon.KSheet:show() end)
-- bindKey('x', function() spoon.KSheet:hide() end)


-- Set correct screen rotation

-- function rotate()
--    thisScreen=hs.mouse.getCurrentScreen()
--    rotation=thisScreen:rotate()
--    thisScreen:rotate((rotation+90) % 360)
-- end


-- bindKey('S', function() rotate() end)

---
--- Detect displays when mash-A is pressed
---
-- function detect_displays()
--    hs.applescript.applescript([[
-- tell application "System Preferences"
--     activate
--     reveal pane "com.apple.preference.displays"
-- end tell

-- delay 0.5

-- tell application "System Events"
--     tell process "System Preferences"
--         try --don't even consider not using a try block!
--             key down option
--             delay 0.2
--             click button "Detect Displays" of window 1
--             delay 0.2
--             key up option
--         on error --logging out is the only other way to clear these
--             key up option
--         end try
--     end tell
--     tell process "System Preferences"
--        quit
--     end tell
-- end tell
-- ]])
-- end

-- bindKey('A', function() detect_displays() end)

-- Rescue Windows
-- Move any windows that are off-screen onto the main screen
function rescueWindows()
    local screen = hs.screen.mainScreen()
    local screenFrame = screen:fullFrame()
    local wins = hs.window.visibleWindows()
    for i,win in ipairs(wins) do
        local frame = win:frame()
        if not frame:inside(screenFrame) then
            win:moveToScreen(screen, true, true)
        end
    end
end

bindKey('R', function() rescueWindows()
end)

bindKey('D', function() hs.execute("open ~/Downloads") end)

function help()
   return [[
     mash + ; - move screen left     mash + \ - move screen right

                            mash + i - move N
     mash + j - move W      mash + k - move centre     mash + l - move E
                            mash + , - move S

     mash + f - fullscreen

     mash + c - cheatsheet     mash + x - exit cheatsheet

     mash + s - rotate 90 degrees     mash + a - detect displays     mash + r - rescue lost windows

     mash + h - help. This screen     mash + d - Open Downloads
]]
end

local helpStyle = {}
helpStyle['textFont'] = 'Courier New'
helpStyle['radius'] = 10
helpStyle['textSize'] = 14


hs.loadSpoon('Caffeine')
spoon.Caffeine:start()


-- utility for next time you want to look at a table.
-- E.g.
-- rPrint(hs.network.interfaceDetails("en8"))

function rPrint(s, l, i) -- recursive Print (structure, limit, indent)
	l = (l) or 100; i = i or "";	-- default item limit, indent string
	if (l<1) then print "ERROR: Item limit reached."; return l-1 end;
	local ts = type(s);
	if (ts ~= "table") then print (i,ts,s); return l-1 end
	print (i,ts);           -- print "table"
	for k,v in pairs(s) do  -- print "[KEY] VALUE"
		l = rPrint(v, l, i.."\t["..tostring(k).."]");
		if (l < 0) then break end
	end
	return l
end

-- hs.loadSpoon('Cherry')
