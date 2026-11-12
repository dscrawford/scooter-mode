# scooter-mode

Use [Scooter](https://github.com/chadac-me/scooter) from Emacs: browse and start
conversations on one or more Scooter servers, chat through
[agent-shell](https://github.com/xenodium/agent-shell), and sign in behind
oauth2-proxy.

Work in progress.

## Emacs

Needs Emacs 29.1+, `magit-section`, `transient`, `agent-shell` and `curl` 7.76+ (shell-maker checks it), plus
the `scooter` binary on `PATH` (`nix profile install github:chadac-me/scooter-mode`).

```elisp
(add-to-list 'load-path "~/src/scooter-mode/lisp")
(require 'scooter)
(require 'scooter-project)          ; C-x p p and Projectile hooks
(server-start)                      ; lets the bridge refresh logins
(keymap-global-set "C-c s" scooter-command-map)

(setq scooter-servers
      '((home :url "https://scooter.example.org" :auth cookie)
        (work :url "https://scooter.corp.example"
              :auth (oidc :issuer "https://oauth.id.jumpcloud.com/"
                          :client-id "scooter-emacs"))))
```

- `C-c s l` lists conversations on every server as a table (status icon,
  title, model, links, owner, last activity), starred ones first. Hover an
  icon for what it means; `TAB` on a row shows its details (id, created, linked PRs and threads, URL). `RET`
  opens one in agent-shell; `c` new, `s` star, `t` retitle, `k` stop the run,
  `z` suspend, `f` files viewer, `b` browser, `g` refresh, `?` menu.
  Shells are named `Scooter <slug>@<server>`; a new conversation's shell
  takes its slug after the turn in which the agent titles it.
- Approvals render in the shell. The agent's questions become an
  agent-shell form (`scooter-agent-shell-forms`, on by default, turns on
  agent-shell's experimental forms in Scooter shells only; off, they
  point at the web UI). AWS access requests show the account, risk,
  policy and approvals still needed above Approve/Deny. Commit identity proposals
  show the identity; kinds with no card here show their raw data and the
  web UI link. A request answered in the web UI is withdrawn from the shell.
- `C-c s b` (`scooter-sandbox`) shows the conversation's sandbox (status,
  size, visibility) and resizes, suspends or resumes, restarts, compacts,
  renames, stars or changes its visibility; `C-c s w` opens, copies, starts
  or stops one of its web services; `C-c s k` opens a linked resource (`C-u`
  copies its URL). Each acts on the Scooter shell you are in or the row at
  point in `scooter-list` (`m`, `v`, `l` there), and `C-u` picks another.
  Links added while a conversation is open, and web services the agent
  starts, show up as notes in its shell.
- `C-c s c`, `C-c s r`, `C-c s p` start a conversation from a prompt, the
  region, or the current project; `C-c s R` sends the region to an existing
  one.
- Auth: `cookie` asks for the `_oauth2_proxy` cookie (or reads it from
  auth-source, host = the server, user = `_oauth2_proxy`) and saves it to
  your first `auth-sources` entry, replacing the old one. If oauth2-proxy
  runs with `--cookie-name`, give it: `(cookie :cookie-name
  "__Host-scooter_oauth2")` sends, asks for and stores that cookie
  instead (host = the server, user = the name). `oidc` logs in
  through the browser with PKCE; oauth2-proxy must run with
  `--skip-jwt-bearer-tokens` and accept the client as an extra JWT issuer.
  GitHub issues no ID tokens, so GitHub-backed servers use `cookie`.

### Signing in through a browser

With `:auth (cookie :browser BROWSER)` the cookie comes from your own browser
instead of being pasted. `BROWSER` is `firefox`, `chromium`, `chrome`,
`brave` or `librewolf` (see `scooter-auth-browsers`), or `t` for all of them:

```elisp
(setq scooter-servers
      '((home :url "https://scooter.example.org" :auth (cookie :browser t))))
```

When a request needs a cookie (none saved yet, or the saved one was refused),
Emacs reads the session cookie (`_oauth2_proxy`, or the `:cookie-name` you
give) from the browser's profiles and checks it against the server's
`/oauth2/auth`. If a browser is already signed in, that is all; otherwise
`<server>/oauth2/start` opens — in that browser for a named one, through
`browse-url` for `t` — and Emacs watches the profiles until the new session
shows up (`scooter-auth-login-timeout`, 5 minutes; `C-g` gives up). Either way
the cookie is saved to auth-source as with plain `cookie`. It never asks you
to paste it. `C-c s s B` (`scooter-auth-browser-login`) does the same on
demand.

- Firefox-family profiles come from `profiles.ini` (the default one first,
  then the rest, then any other directory there holding a `cookies.sqlite`);
  Chromium-family ones from the user data directory's `Local State` (the
  last used first). The directories searched are in `scooter-auth-browsers`,
  with XDG, Snap and Flatpak locations; set `scooter-auth-browser-profile` to
  read one profile only. The databases are copied before reading, so a
  running browser is fine. Needs Emacs built with sqlite
  (`(sqlite-available-p)`). Without it, or with no profile at all, you get an
  error saying so; use plain `cookie` if you want to paste the value.
- Chromium encrypts its cookies. With its basic password store (the usual
  thing outside GNOME/KDE sessions) they're decrypted right here; with the
  keyring (`v11` values) the key is read from the Secret Service item
  `"Chromium Safe Storage"`/`"Brave Safe Storage"`/… through `secrets.el`,
  so Emacs needs D-Bus and an unlocked keyring. Cookies that can't be
  decrypted are skipped, and a timed-out sign-in says how many. Chromium on
  macOS keeps its key in the Keychain and isn't read.
- Cookies for the server's host and its parent domains count (a
  `--cookie-domain=.example.org` session works), and a session oauth2-proxy
  split into `_oauth2_proxy_0`, `_1`, … is joined back into one value. A
  server whose oauth2-proxy runs with `--cookie-name` needs that name in
  `:cookie-name`, e.g. `(cookie :browser t :cookie-name
  "__Host-scooter_oauth2")`; otherwise Emacs waits for a cookie that never
  appears.
- Each browser's `:program` opens the page (`browse-url-firefox-program`,
  `browse-url-chromium-program`, `brave`, …); when it isn't on `PATH`,
  `browse-url` does. If the server doesn't serve oauth2-proxy's
  `/oauth2/auth`, point `scooter-auth-browser-check-path` at a path that
  answers 2xx only when signed in.
- One sign-in runs at a time: while Emacs waits, other shells' bridges are
  told to retry and wait for it. A sign-in that times out or is cancelled
  with `C-g` isn't repeated until your next command, so one `scooter-kanban`
  waits once, not once per request it makes; background work (change
  streams, timed refreshes) never opens the sign-in page at all.

### Kanban boards

`C-c s K` (`scooter-kanban`) shows the boards of the server's kanban
contrib, the same boards as the web UI's /boards:

```
home  https://scooter.example.org  1 board
Work  5 cards
  Design (3)
    ● Write docs
      You · 2h ago
    ● Fix login HIGH dotfiles
      PR #4 open, CI green · Agent · 5m ago · Blocked by 1
  In progress (1)
    ! Kanban in Emacs URGENT
      No status yet
  Done (1)
  Tags and skills (1, 1)
```

Each stage lists its cards in board order: status icon, title, High or
Urgent, tags, the agent's note, who moved it last and what still blocks
it. The icons of the cards waiting on you, working or failed roll up to
the stage and board headings, so a folded stage still shows what is going
on inside it. Boards, stages and cards fold like an outline: `TAB` folds the one at
point, `M-2` folds everything to boards, `M-3` to stages and `M-4` shows
every card; a card folds open to its description, blockers, links and URL.
The boards start folded (`scooter-kanban-fold-boards`), and a fold stays as
you left it when the board refreshes.
`RET` opens the card's conversation in agent-shell in another window and
keeps the board in view (`o` shows it without leaving the board, `]`/`[`
step through the cards). From a Scooter shell, `scooter-kanban` shows the
board beside it with point on that conversation's card.

To open each card's chat in a tab of its own instead, set
`scooter-kanban-open-in` to `tab`. The tab is named after the card, `RET`
on the card again switches to it, and closing the tab closes the chat's
shell (the conversation stays on the server; `RET` reopens it). `o` opens
the tab in the background, and `C-c C-k` from a chat goes back to the
board's tab. `]`/`[` still show cards in a window, so stepping through
them doesn't open a tab for each.

- Cards: `>`/`<` next or previous stage, `m` any stage, `M-<up>`/`M-<down>`
  reorder, `c` new conversation in the stage at point, `a` put an
  existing conversation there, `u` take it off the board, `#` tags (new
  names make tags), `P` priority, `e` description, `x`/`X` add or remove a
  blocker. A move to another stage tells the agent, as the web UI does
  (`scooter-kanban-announce-moves`).
- The list's conversation keys work on cards too: `s` star, `t` retitle,
  `k` stop, `z` suspend, `C-c C-b` sandbox, `v` web services, `l` links,
  `b` browser, `w` copy URL.
- Boards: `N` new, `R` rename, `D` delete; `E` edits the stages, tags and
  skills of the board at point. `?` lists everything.

After each server's boards come its **Skills**, your personal skills
(Settings → Skills on the web: a name, when to use it, and markdown
instructions, loaded by every conversation of yours), and its **Hooks**, the
deployment's hooks with the ones you may turn off:

```
Skills  2
  ● acme-conventions
    Conventions for acme/app. Use when working in it or reviewing its PRs.
  ○ old-notes  disabled
Hooks  1
  ● finish-or-ask  Stop  required
    Send the agent back to work instead of ending.
```

A skill is edited as the file the agent will see, in a window of its own:
front matter with its name and details, then the markdown body. `i` starts
a new personal skill on the server at point, and `RET` or `e` on a skill
opens it:

```
---
name: acme-conventions
description: Conventions for acme/app. Use when working in it or reviewing its PRs.
enabled: true
---

# acme/app

Run `just ci` before opening a PR.
```

`C-c C-c` submits it, creating or updating the skill from the front matter
and body (`C-c C-k` discards; the buffer stays open if the server refuses
it, with its reason). A board's skills (`E s` to add, `e` on one to edit)
open the same way, with only a `name` line, since every card on the board
gets them. `!` enables or disables the personal skill or hook at point
(`RET` on a hook does too); `E K` deletes a personal skill. The server has
no way to write hooks, so Emacs only lists and toggles them. Changes apply
to conversations started afterwards.
- `C-c s M` (`scooter-kanban-file`) puts the current shell's conversation
  on a board.

The buffer follows `/conversations/events`, and redraws when a change
arrives while it is in view (or as soon as it comes back into view).

### Sandbox files over Tramp

`C-c s f` (`scooter-find-file`) opens a file in a conversation's sandbox:

```
/scooter:<conversation>@<server>:/workspace/...
```

`<conversation>` is the title slug shown in `scooter-list` (or an id prefix),
and both parts complete. In a Scooter agent-shell buffer, `C-x C-f`,
`C-c C-f` and `C-c s f` start in that conversation's sandbox (`C-u C-c s f`
asks), and `C-c C-j` opens Dired on its `/workspace` (agent-shell's
`C-c C-d` stays comint's send-EOF). The keys live in
`scooter-agent-shell-files-mode-map`, so `keymap-set` rebinds them. Paths
like `/workspace/foo.mjs:42` (also `:42:5` and `#L42`) in the agent's text
and tool output become links: `RET` or a click opens the file in the sandbox
at that line (`scooter-agent-shell-link-paths` turns this off). Tramp runs `ssh` with `scooter tunnel` as its
ProxyCommand, which wakes the sandbox, starts its `ssh` web service and pipes
sshd over a WebSocket. The sandbox needs `webServices.ssh.enable`, and `ssh`
must be on your `PATH`. Before each connect Emacs writes the conversation id
and current auth headers to a 0600 file under `scooter-tramp-directory`, next
to the client key.

## `scooter tunnel`

`scooter tunnel --server URL --conversation ID --headers-command CMD` pipes
stdin/stdout to a conversation's sshd, for use as an ssh `ProxyCommand`.
`--spec-file FILE` reads the server, conversation and headers from one JSON
file instead: `{"url": …, "conversation": …, "headers": {…}}`.
While the ssh service doesn't answer (the proxy's status poll fails with a
5xx, says it isn't running, or the upgrade gets a 5xx), it resumes the
conversation if it isn't running, starts the sandbox's `ssh` web service and
retries, for up to `--wait` (default 5m). From a shell:

```sh
ssh -o ProxyCommand="scooter tunnel --server https://scooter.example.org --conversation <id> \
      --headers-command 'pass show scooter/headers'" root@sandbox
```

## `scooter acp`

`scooter acp --server https://scooter.example.org` is an
[ACP](https://agentclientprotocol.com) agent on stdin/stdout. A new ACP session
creates a Scooter conversation; `--conversation <id>` attaches the first one to
an existing conversation instead. `session/list` returns your top-level
conversations in the web sidebar's order (starred, then most recently active),
with their status, star, visibility and linked sources under `_meta.scooter`;
`session/resume` attaches without replaying history, and `session/close` stops
following a conversation. Sessions offer a `model` config option from the
server's `/models`; once you pick one, prompts go through `/agui` with
`X-Agent-Model`, as the web UI sends them (the conversation's own model needs
no pick and keeps `/messages`). Image prompts (png, jpeg, gif, webp, up to 5 MB) go
through `/agui` too, and images in history and live runs (yours, the agent's,
and tool results') are fetched from the conversation's assets and inlined. A run
started elsewhere (the web UI, a webhook, a schedule) arrives as a
`session/push`, its prompt included, and ends with `session_push_end`; while
any run is going, `_session/steering` adds your prompt to it, as the web UI's
interrupt does. `--replay-turns N` (`scooter-acp-replay-turns` in Emacs) replays
only the last N turns of a long conversation. Auth headers come from
`--headers-command` (or `--headers-file`), as a JSON object such as
`{"Authorization": "Bearer …"}` or `{"Cookie": "_oauth2_proxy=…"}`. The command
is rerun once when the server's auth proxy rejects a request, with `{retry}`
in it replaced by `t` (otherwise `nil`): first without it, in case the
headers already changed, and with it only when they didn't. Requests refused
at the same time share that one rerun, so an expired session asks you to
sign in once, then the refused request is retried and the event streams
reconnect. If the command prints `null`, a sign-in is already open elsewhere
(another shell's Emacs prompt) and the bridge asks again shortly rather than
prompting twice. Requests other than streams give up after
`--request-timeout` (15s) and name the server.

Prebuilt static binaries for Linux and macOS (amd64, arm64) are attached to
each [GitHub release](https://github.com/chadac-me/scooter-mode/releases), with
`SHA256SUMS`; `scooter version` prints the release. Or run it with Nix:

```sh
nix run github:chadac-me/scooter-mode -- acp --server https://scooter.example.org \
  --headers-command 'pass show scooter/headers'
```
