alias fzfp="fzf --preview 'bat --style=numbers --color=always --line-range :500 {}'"
alias myvm="et -c "tmux -CC new -A colin-vm" colin@colin-vm.dev.us-west-1.nexthop.ai"

# first:
# uv python install 3.14
# Installed Python 3.14.0a4 in 1.87s
# then
alias python3.14='uv run --python=3.14 python3'

# check my startup time.
alias timezsh='for i in $(seq 1 10); do time zsh -i -c exit; done'

