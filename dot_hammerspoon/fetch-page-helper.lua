-- Path discovery: hs.task does NOT go through a shell, so PATH isn't consulted.
local EMACSCLIENT = (function()
    for _, p in ipairs({
        "/opt/homebrew/bin/emacsclient",
        "/usr/local/bin/emacsclient",
        "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient",
        "/usr/bin/emacsclient",
    }) do
        local f = io.open(p, "r")
        if f then f:close(); return p end
    end
    hs.alert.show("emacsclient not found"); return nil
end)()

-- Encode any string as elisp literal: sidestep all quoting concerns by
-- base64-ing on this side and decoding on the elisp side.
local function lit(s)
    if not s or s == "" then return '""' end
    return '(base64-decode-string "' .. hs.base64.encode(s) .. '")'
end

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

-- Browser classification.
local CHROMIUM_APPS = {
    ["Google Chrome"]=true, ["Google Chrome Canary"]=true, ["Chromium"]=true,
    ["Brave Browser"]=true, ["Microsoft Edge"]=true, ["Vivaldi"]=true,
    ["Arc"]=true,
}
local function frontBrowser()
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
    local titleProp =
        (appName == "Safari" or appName == "Safari Technology Preview")
        and "name" or "title"
    local script = string.format([[
    tell application "%s"
        set t to %s
        return (URL of t) & linefeed & (%s of t)
    end tell
]], appName, tabExpr, titleProp)    local ok, out = hs.osascript.applescript(script)
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

-- Dispatch helpers.
local function getURLAndTitle(kind, name)
    if kind == "safari"   then return safariURLAndTitle(name)
    elseif kind == "chromium" then return chromiumURLAndTitle(name)
    elseif kind == "firefox"  then return firefoxURL(), firefoxTitle()
    end
end

local function fire(url, title, body)
    if not EMACSCLIENT then return end
    if not url or url == "" then
        local kind, name = frontBrowser()
        hs.alert.show(string.format(
            "Capture: no URL from %s (%s)", name or "?", kind or "unknown"))
        print("captureLink failure — front app:", name, "kind:", kind)
        return
    end
    local elisp = string.format("(my/org-capture-web %s %s %s)",
        lit(url), lit(title or ""), lit(body or ""))
    hs.task.new(EMACSCLIENT, function(rc, _, err)
        if rc ~= 0 then hs.alert.show("Capture failed: "..(err or rc)) end
    end, {"-e", elisp}):start()
    hs.application.launchOrFocus("Emacs")
end

-- Variant 1: URL + title only.
local function captureLink()
    local kind, name = frontBrowser()
    if not kind then hs.alert.show("Not a known browser"); return end
    fire(getURLAndTitle(kind, name))  -- 3rd arg defaults to nil → empty body
end

-- Variant 2: URL + title + selection. IMPORTANT: grab the selection FIRST,
-- before we start doing anything to the address bar (Firefox case).
local function captureLinkWithSelection()
    local kind, name = frontBrowser()
    if not kind then hs.alert.show("Not a known browser"); return end
    local selection = withClipboardPreserved(function()
        return copyAndWait(500) or ""
    end)
    local url, title = getURLAndTitle(kind, name)
    fire(url, title, selection)
end

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "j", captureLink)
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "k", captureLinkWithSelection)
