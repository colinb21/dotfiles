-- ============================================================
--  Org-capture from Google Chrome → Emacs
--  Drop into ~/.hammerspoon/init.lua (or require from there)
-- ============================================================

-- ── Configuration ────────────────────────────────────────────
local ORG_CAPTURE_KEY    = { {"cmd", "alt"}, "c" }   -- hotkey: Cmd+Alt+C
local ORG_CAPTURE_TEMPLATE = "g"                      -- your capture template key
-- local ORG_INBOX_FILE     = "~/org/inbox.org"          -- fallback, used in template
local EMACSCLIENT        = "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"

-- ── Helpers ──────────────────────────────────────────────────

-- Escape a string for safe embedding in a double-quoted Elisp string
local function elispEscape(s)
  s = s:gsub('\\', '\\\\')   -- backslashes first
  s = s:gsub('"',  '\\"')    -- double-quotes
  return s
end

-- Run a shell command and return stdout (trimmed)
local function shellOut(cmd)
  local handle = io.popen(cmd)
  local result = handle:read("*a")
  handle:close()
  return result:match("^%s*(.-)%s*$")  -- trim whitespace
end

-- ── Main capture function ─────────────────────────────────────
local function captureGoogleDoc()

  -- 1. Grab title + URL from the frontmost Chrome tab
  local script = [[
    tell application "Google Chrome"
      set t to title of active tab of front window
      set u to URL of active tab of front window
      return t & "\n" & u
    end tell
  ]]

  local ok, result, raw = hs.osascript.applescript(script)

  if not ok then
    hs.notify.new({
      title    = "Org Capture",
      informativeText = "Could not read Chrome tab.\nIs Chrome running with a window open?",
      withdrawAfter = 5,
    }):send()
    return
  end

  -- AppleScript returns a single string; split on the newline
  local title, url = result:match("^(.-)\n(.+)$")

  if not title or not url then
    hs.notify.new({
      title    = "Org Capture",
      informativeText = "Unexpected AppleScript output:\n" .. tostring(result),
      withdrawAfter = 5,
    }):send()
    return
  end

  -- 2. Optionally skip non-Google-Doc URLs (comment out to capture any page)
  local googleDocPattern = "https://docs%.google%.com/"
  if not url:match(googleDocPattern) then
    hs.notify.new({
      title    = "Org Capture — skipped",
      informativeText = "Frontmost tab doesn't look like a Google Doc:\n" .. url,
      withdrawAfter = 4,
    }):send()
    -- Remove the return below if you want to capture ANY Chrome tab, not just Google Docs
    return
  end

  -- 3. Build an org-mode link:  [[url][title]]
  local orgLink = string.format("[[%s][%s]]", url, title)

  -- 4. Build the Elisp expression to send to Emacs
  --    We call org-capture-string which feeds the link into your template.
  local elisp = string.format(
    '(org-capture-string "%s" "%s")',
    elispEscape(orgLink),
    ORG_CAPTURE_TEMPLATE
  )

  -- 5. Send to running Emacs via emacsclient
  local cmd = string.format(
    '%s --no-wait -e %q 2>&1',
    EMACSCLIENT,
    elisp
  )

  local output = shellOut(cmd)

  if output ~= "" and not output:match("^%s*$") then
    -- emacsclient prints to stderr on error; surface it
    hs.notify.new({
      title    = "Org Capture — error",
      informativeText = output,
      withdrawAfter = 8,
    }):send()
  else
    hs.notify.new({
      title    = "Org Capture ✓",
      informativeText = title,
      withdrawAfter = 3,
    }):send()
  end
end

-- ── Bind the hotkey ──────────────────────────────────────────
hs.hotkey.bind(ORG_CAPTURE_KEY[1], ORG_CAPTURE_KEY[2], captureGoogleDoc)

hs.notify.new({
  title = "Hammerspoon",
  informativeText = "Org-capture loaded — " ..
    table.concat(ORG_CAPTURE_KEY[1], "+") .. "+" .. ORG_CAPTURE_KEY[2],
  withdrawAfter = 3,
}):send()
