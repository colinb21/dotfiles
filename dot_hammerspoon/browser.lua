-- Shared front-browser URL/title/selection helpers for Hammerspoon scripts.
local M = {}

-- Save+restore the pasteboard around an operation. Only preserves the
-- primary string. Images, files, rich-types are lost. Acceptable tradeoff.
local function withClipboardPreserved(fn)
    local saved = hs.pasteboard.getContents()
    local result = fn()
    if saved then hs.pasteboard.setContents(saved) end
    return result
end

-- Cmd+C, then wait for changeCount to tick. Returns nil on timeout
-- (which is also how we detect "no selection").
local function copyAndWait(timeoutMs)
    timeoutMs = timeoutMs or 500
    local before = hs.pasteboard.changeCount()
    hs.eventtap.keyStroke({"cmd"}, "c", 0)
    local deadline = hs.timer.absoluteTime() / 1e6 + timeoutMs
    while hs.pasteboard.changeCount() == before do
        if hs.timer.absoluteTime() / 1e6 > deadline then return nil end
        hs.timer.usleep(10000)
    end
    return hs.pasteboard.getContents()
end

-- Grab the current selection as a string ("" if none), leaving the
-- pasteboard untouched afterwards.
function M.getSelection(timeoutMs)
    return withClipboardPreserved(function()
        return copyAndWait(timeoutMs or 500) or ""
    end)
end

-- Browser classification.
local CHROMIUM_APPS = {
    ["Google Chrome"]=true, ["Google Chrome Canary"]=true, ["Chromium"]=true,
    ["Brave Browser"]=true, ["Microsoft Edge"]=true, ["Vivaldi"]=true,
    ["Arc"]=true,
}

-- Returns kind ("safari"|"chromium"|"firefox"|nil), appName.
function M.frontBrowser()
    local app = hs.application.frontmostApplication()
    if not app then return nil end
    local name = app:name()
    if name == "Safari" or name == "Safari Technology Preview" then
        return "safari", name
    elseif CHROMIUM_APPS[name] then
        return "chromium", name
    elseif name:match("^Firefox") then
        return "firefox", name
    end
    return nil, name
end

-- Safari & Chromium: just ask. Both return URL\nTITLE.
local function appleScriptURLTitle(appName, tabExpr)
    local titleProp =
        (appName == "Safari" or appName == "Safari Technology Preview")
        and "name" or "title"
    local script = string.format([[
    tell application "%s"
        set t to %s
        return (URL of t) & linefeed & (%s of t)
    end tell
]], appName, tabExpr, titleProp)
    local ok, out = hs.osascript.applescript(script)
    if not ok or type(out) ~= "string" then return nil, nil end
    return out:match("([^\n]*)\n(.*)")
end

local function safariURLAndTitle(name)
    return appleScriptURLTitle(name, "front window's current tab")
end
local function chromiumURLAndTitle(name)
    return appleScriptURLTitle(name, "active tab of front window")
end

-- Firefox: window title for the page title (with suffix stripping),
-- address-bar keystroke for the URL.
local FIREFOX_SUFFIXES = {
    " — Mozilla Firefox", " – Mozilla Firefox", " - Mozilla Firefox",
    " — Firefox Developer Edition", " — Firefox Nightly", " — Firefox",
}
local function firefoxTitle()
    local win = hs.window.frontmostWindow()
    if not win then return "" end
    local t = win:title() or ""
    for _, sfx in ipairs(FIREFOX_SUFFIXES) do
        if t:sub(-#sfx) == sfx then return t:sub(1, -#sfx - 1) end
    end
    return t
end

local function firefoxURL()
    return withClipboardPreserved(function()
        local before = hs.pasteboard.changeCount()
        hs.eventtap.keyStroke({"cmd"}, "l", 0)   -- focus and select URL bar
        hs.timer.usleep(80000)                    -- field needs a moment
        hs.eventtap.keyStroke({"cmd"}, "c", 0)
        local deadline = hs.timer.absoluteTime() / 1e6 + 500
        while hs.pasteboard.changeCount() == before do
            if hs.timer.absoluteTime() / 1e6 > deadline then break end
            hs.timer.usleep(10000)
        end
        local url = hs.pasteboard.getContents() or ""
        hs.eventtap.keyStroke({}, "escape", 0)    -- close awesomebar dropdown
        -- Sanity-check: looks like a URL?
        if url:match("^%a[%w+.-]*:") then return url end
        return ""
    end)
end

-- Dispatch: returns URL, TITLE for the given browser kind/name.
function M.getURLAndTitle(kind, name)
    if kind == "safari"   then return safariURLAndTitle(name)
    elseif kind == "chromium" then return chromiumURLAndTitle(name)
    elseif kind == "firefox"  then return firefoxURL(), firefoxTitle()
    end
end

return M
