-- Shared Emacs/emacsclient plumbing for Hammerspoon scripts.
local M = {}

-- Path discovery: hs.task does NOT go through a shell, so PATH isn't consulted.
M.emacsclient = (function()
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

-- Encode any string as an elisp literal: sidestep all quoting concerns by
-- base64-ing on this side and decoding on the elisp side.
function M.lit(s)
    if not s or s == "" then return '""' end
    return '(base64-decode-string "' .. hs.base64.encode(s) .. '")'
end

-- Evaluate ELISP via `emacsclient -e`, then bring Emacs to the front.
-- ONERR (optional) is called with a message string on non-zero exit.
function M.eval(elisp, onErr)
    if not M.emacsclient then return end
    hs.task.new(M.emacsclient, function(rc, _, err)
        if rc ~= 0 and onErr then onErr(err or tostring(rc)) end
    end, {"-e", elisp}):start()
    hs.application.launchOrFocus("Emacs")
end

return M
