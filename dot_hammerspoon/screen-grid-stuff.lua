local LARGE_GRID = '3x3'
local SMALL_GRID = '2x2'

-- Per-screen overrides for runtime toggling (keyed by screen UUID)
local gridOverrides = {}

-- Default grid for a given screen. Built-in MBP display -> 2x2,
-- everything else (Thunderbolt/Studio/etc.) -> 3x3.
local function defaultGridFor(screen)
    local name = screen:name() or ""
    if name:match("Built[- ]?in") then
        return SMALL_GRID
    end
    -- Width-based fallback in case the name match ever fails
    if screen:frame().w < 1800 then
        return SMALL_GRID
    end
    return LARGE_GRID
end

local function applyGrid(screen)
    local uuid = screen:getUUID()
    local grid = (uuid and gridOverrides[uuid]) or defaultGridFor(screen)
    hs.grid.setGrid(grid, screen)
end

local function applyAllGrids()
    for _, screen in ipairs(hs.screen.allScreens()) do
        applyGrid(screen)
    end
end

-- Toggle 2x2 <-> 3x3 on whichever screen the focused window lives on
bindKey('g', function()
    local fw = hs.window.focusedWindow()
    local screen = (fw and fw:screen()) or hs.screen.mainScreen()
    local uuid = screen:getUUID()
    local current = gridOverrides[uuid] or defaultGridFor(screen)
    gridOverrides[uuid] = (current == LARGE_GRID) and SMALL_GRID or LARGE_GRID
    applyGrid(screen)
    hs.alert.show(('Grid on %s: %s'):format(screen:name(), gridOverrides[uuid]))
end)


bindKey('l', function() hs.grid.show() end)

applyAllGrids()

-- Re-apply whenever displays are plugged/unplugged or resolution changes
screenWatcher = hs.screen.watcher.new(applyAllGrids)
screenWatcher:start()
