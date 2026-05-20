hs.alert.show("Config loaded")
--hs.loadSpoon("WinWin")
--hs.loadSpoon("WindowHalfsAndThirds")

local cac = {"cmd", "alt", "ctrl"}

function bindKey(key, fn)
  hs.hotkey.bind(cac, key, fn)
end

 -- Keybindings cheat-sheet for current application. Press to toggle.
hs.loadSpoon("KSheet")
bindKey('c', function() spoon.KSheet:toggle() end)

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

-- stay awake
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

require "fetch-page-helper"
require "screen-grid-stuff"
