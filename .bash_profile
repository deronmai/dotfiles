alias ll="ls -liah"
alias pip="pip3"
export PATH="/usr/local/opt/llvm/bin:$HOME/Library/Python/3.7/bin:$PATH"

powerline-daemon -q
POWERLINE_BASH_CONTINUATION=1
POWERLINE_BASH_SELECT=1
. /Users/deron/Library/Python/3.7/lib/python/site-packages/powerline/bindings/bash/powerline.sh

(lsp-clients-register-clangd)
export LDFLAGS="-L/usr/local/opt/llvm/lib"
export CPPFLAGS="-I/usr/local/opt/llvm/include"
