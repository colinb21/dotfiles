-- Jump from a JIRA ticket in the front browser tab to its Denote note in
-- Emacs. Mirror of the Emacs-side `C-c n o` (note → browser).
local browser = require "browser"
local emacs = require "emacs"

-- Extract a JIRA key (e.g. NOS-9140) from a URL. Prefer the canonical
-- /browse/KEY form; fall back to a key anywhere in the URL (board views
-- use ...?selectedIssue=KEY and similar).
local function jiraKeyFromURL(url)
    if not url then return nil end
    return url:match("/browse/([A-Z][A-Z0-9]+%-%d+)")
        or url:match("([A-Z][A-Z0-9]+%-%d+)")
end

local function jumpToDenote()
    local kind, name = browser.frontBrowser()
    if not kind then hs.alert.show("Not a known browser"); return end
    local url = browser.getURLAndTitle(kind, name)
    local key = jiraKeyFromURL(url)
    if not key then hs.alert.show("No JIRA key in front tab URL"); return end
    local elisp = string.format("(my-denote-jira-visit-by-key %s)", emacs.lit(key))
    emacs.eval(elisp, function(e) hs.alert.show("Denote jump failed: " .. e) end)
end

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "n", jumpToDenote)
