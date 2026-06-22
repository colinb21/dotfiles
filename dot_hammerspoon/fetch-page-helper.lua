-- Capture the front browser tab (URL + title, optionally selection) into
-- Emacs via org-capture. Browser and emacsclient plumbing live in the
-- shared `browser` and `emacs` modules.
local browser = require "browser"
local emacs = require "emacs"

local function fire(url, title, body)
    if not url or url == "" then
        local kind, name = browser.frontBrowser()
        hs.alert.show(string.format(
            "Capture: no URL from %s (%s)", name or "?", kind or "unknown"))
        print("captureLink failure — front app:", name, "kind:", kind)
        return
    end
    local elisp = string.format("(my/org-capture-web %s %s %s)",
        emacs.lit(url), emacs.lit(title or ""), emacs.lit(body or ""))
    emacs.eval(elisp, function(e) hs.alert.show("Capture failed: " .. e) end)
end

-- Variant 1: URL + title only.
local function captureLink()
    local kind, name = browser.frontBrowser()
    if not kind then hs.alert.show("Not a known browser"); return end
    fire(browser.getURLAndTitle(kind, name))  -- 3rd arg defaults to nil → empty body
end

-- Variant 2: URL + title + selection. IMPORTANT: grab the selection FIRST,
-- before we start doing anything to the address bar (Firefox case).
local function captureLinkWithSelection()
    local kind, name = browser.frontBrowser()
    if not kind then hs.alert.show("Not a known browser"); return end
    local selection = browser.getSelection(500)
    local url, title = browser.getURLAndTitle(kind, name)
    fire(url, title, selection)
end

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "j", captureLink)
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "k", captureLinkWithSelection)
