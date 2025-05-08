if !has('nvim')
  source $VIMRUNTIME/defaults.vim
endif


" == Helper function
function! s:mycapture(excmd) abort
  try
    redir => out
    exe 'silent! '.a:excmd
  finally
    redir END
  endtry
  return out
endfunction

" Check if coming from ssh
let g:remoteSession = ($STY == "")

" == Default font.  Type :set guifont=*   to see available fonts
if has("gui_running")
    if has("mac")
        set guifont=Menlo:h16
    else
        set guifont=Consolas:h16
    endif
endif

" == Basic See: https://www.tutorialdocs.com/article/vim-configuration.html
set encoding=utf-8
set showmode                " Display the current mode
set mouse=a                 " Automatically enable mouse usage
set mousehide               " Hide the mouse cursor while typing
filetype plugin indent on   " Automatically detect file types.
syntax on                   " Syntax highlighting

if has('clipboard')
    if has('unnamedplus')  " When possible use + register for copy-paste
       set clipboard=unnamed,unnamedplus
    else         " On mac and Windows, use * register for copy-paste
        set clipboard=unnamed
    endif
endif

if has('persistent_undo')            "check if your vim version supports
  set undodir=$HOME/.cache/vim/undo  "directory where the undo files will be stored
  set undofile                       "turn on the feature
endif

" == Searching
set number                      " Line numbers on
set showmatch                   " Show matching brackets/parenthesis
set incsearch                   " Find as you type search
set hlsearch                    " Highlight search terms
set matchtime=0
set cpoptions+=x
set winminheight=0              " Windows can be 0 line high
set ignorecase                  " Case insensitive search
set smartcase                   " Case sensitive when uc present
set scrolljump=5                " Lines to scroll when cursor leaves screen
set scrolloff=3                 " Minimum lines to keep above and below cursor

" == Formatting and Indentation
set backspace=indent,eol,start  " Backspace for dummies
set linespace=0                 " No extra spaces between rows
set autoindent                  " Indent at the same level of the previous line
set shiftwidth=4                " Use indents of 4 spaces
set expandtab                   " Tabs are spaces, not tabs
set tabstop=4                   " An indentation every four columns
set softtabstop=4               " Let backspace delete indent
set nojoinspaces                " Prevents inserting two spaces after punctuation on a join (J)
set splitright                  " Puts new vsplit windows to the right of the current
set splitbelow                  " Puts new split windows to the bottom of the current
"set matchpairs+=<:>             " Match, to be used with %
"set pastetoggle=<F12>           " pastetoggle (sane indentation on pastes)
set foldenable                  " Auto fold code
set list
set listchars=tab:›\ ,trail:•,extends:#,nbsp:. " Highlight problematic whitespace
syn match markdownError "\w\@<=\w\@=" " Don't flag undescore as an error

" == Autocomplete
" Ctrl-X Ctrl-L  line based completion (:help ins-completion)
" Ctrl-X Ctrl-F  filename completion
" Ctrl-X Ctrl-K  dictionary completion
" Ctrl-X Ctrl-T  thesaurus competion
" Ctrl-N         completion in insertmode
" Ctrl-P         completion in insertmode

" == Edit
set spell spelllang=en_us
set nospell
set undofile
set autochdir
set visualbell
set history=1000
set autoread
set wildmenu                    " Show list instead of just completing
set wildmode=list:longest,full  " Command <Tab> completion, list matches, then longest common part, then all.
set wrap                        " Wrap long lines
set whichwrap=b,s,h,l,<,>,[,]   " Backspace and cursor keys wrap too
set hidden                          " Allow buffer switching without saving
set iskeyword-=.                    " '.' is an end of word designator
set iskeyword-=#                    " '#' is an end of word designator
set iskeyword-=-                    " '-' is an end of word designator

:autocmd FileType text setlocal nocindent noautoindent nospell

" Remember position of last edit and return on reopen
if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

" == GUI
if has('gui_running')
    set guioptions-=T           " Remove the toolbar
    set lines=40                " 40 lines of text instead of 24
else
    if &term == 'xterm' || &term == 'screen' || &term == 'xterm-256color'
        set t_Co=256            " Enable 256 colors to stop the CSApprox warning and make xterm vim shine
    else
        set termguicolors
    endif
endif


" == colorscheme
set background=dark



" See https://stackoverflow.com/questions/5172323/how-to-properly-extend-a-highlighting-group-in-vim
highlight lspInlayHintsType cterm=italic gui=italic
highlight clear MatchParen
highlight MatchParen        cterm=underline,bold gui=underline,bold

" == Appearance
if has('cmdline_info')
    set ruler                   " Show the ruler
    set rulerformat=ZZZ%30(%=\:b%n%y%m%r%w\ %l,%c%V\ %P%) " A ruler on steroids
    set showcmd                 " Show partial commands in status line and
                                " Selected characters/lines in visual mode
endif

if has('statusline')
    set laststatus=2

    set statusline=
    set statusline+=%7*\[%n]                              "buffernr
    set statusline+=%1*\ %<%F\                            "File+path
    set statusline+=%2*\ %y\                              "FileType
    set statusline+=%3*\ %{''.(&fenc!=''?&fenc:&enc).''}  "Encoding
    set statusline+=%3*\ %{(&bomb?\",BOM\":\"\")}\        "Encoding2
    set statusline+=%4*\ %{&ff}\                          "FileFormat (dos/unix..) 
    set statusline+=%5*\ %{&spelllang}\ "Spellanguage
    set statusline+=%8*\ %=\ row:%l/%L\ (%03p%%)\         "Rownumber/total (%)
    set statusline+=%9*\ col:%03c\                        "Colnr
    set statusline+=%0*\ \ %m%r%w\ %P\ \                  "Modified? Readonly? Top/bot.
    set statusline+=%o                                    "Character offset
endif

" == Misc shortcuts
" Shift-Tab to Outdent
" for command mode
nnoremap <S-Tab> <<
" for insert mode
inoremap <S-Tab> <C-d>
" Show character count g-<C-G>
nnoremap ;gg           G$g<C-G>''
" nvim remaps Y to y$ which doesn't include the newline. Annoying.
nnoremap Y Y
" Same with p and P
nnoremap P P
nnoremap p p

" Clear search highlight
" This unsets the "last search pattern" register by hitting return. Keep
" comment on separate line
nnoremap <silent> <CR> :noh<CR><CR>

" Redirect change operations to the _ blackhole to avoid spoiling 'y' register content
nnoremap c "_c
nnoremap C "_C
nnoremap s "_s
nnoremap S "_S

" Change Ctrl-G to display full path to file
nnoremap <c-g> 1<c-g>

" Command-Q --> Noop to avoid accidentally closing window
nnoremap <D-q> <Nop>
inoremap <D-q> <Nop>

" Command-V --> Paste
imap <D-v> :set paste<Enter>+:set nopaste<Enter>
" Alternative
" imap <D-V> "+p
function! SynStack ()
    for i1 in synstack(line("."), col("."))
        let i2 = synIDtrans(i1)
        let n1 = synIDattr(i1, "name")
        let n2 = synIDattr(i2, "name")
        echo n1 "->" n2
    endfor
endfunction
map gm :call SynStack()<CR>


" For Emacs-style editing on the command-line:  :help emacs-keys
" start of line
:cnoremap <C-A> <Home>
" back one character
:cnoremap <C-B> <Left>
" delete character under cursor
:cnoremap <C-D> <Del>
" end of line
:cnoremap <C-E> <End>
" forward one character
:cnoremap <C-F> <Right>
" recall newer command-line
:cnoremap <C-N> <Down>
" recall previous (older) command-line
:cnoremap <C-P> <Up>
" back one word
:cnoremap <Esc><C-B> <S-Left>
" forward one word
:cnoremap <Esc><C-F> <S-Right>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MULTIPURPOSE TAB KEY
" Indent if we're at the beginning of a line. Else, do completion.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! InsertTabWrapper()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<tab>"
    else
        return "\<c-p>"
    endif
endfunction
inoremap <expr> <tab> InsertTabWrapper()
inoremap <s-tab> <c-n>

" Grep: https://thoughtbot.com/blog/faster-grepping-in-vim
if executable('rg')
  " Use rg over grep
  set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case
endif
" bind K to grep word under cursor
nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>

" Tabs: https://vim.fandom.com/wiki/Open_every_buffer_in_its_own_tabpage
nnoremap <C-Left>  :bprevious<CR>
nnoremap <C-Right> :bnext<CR>
" Smart way to move between panes
map <S-Up> <C-w><Up>
map <S-Down> <C-w><Down>
map <S-Left> <C-w><Left>
map <S-Right> <C-w><Right>


" == LSP
if has('nvim')
lua <<EOF
  scriptFolder = vim.fn.fnamemodify(vim.fn.resolve(vim.fn.expand('<sfile>:p')), ':h')
  vim.opt.rtp:prepend(scriptFolder .. "/nvim/")
  dofile(scriptFolder .. '/nvim/_init.lua')
  vim.opt.rtp:prepend(scriptFolder .. "/nvim/")
  dofile(scriptFolder .. '/nvim/_lsp.lua')
  dofile(scriptFolder .. '/nvim/_custom.lua')
EOF

else
  if &term == 'xterm-256color'
    " More pleasant color scheme when you ssh to macos from windows
    if g:remoteSession
        colorscheme desert
    else
        colorscheme solarized8
    endif
  else
    colorscheme darkblue
  endif
  """ Run :PlugInstall
  let s:mypath = fnamemodify(resolve(expand('<sfile>:p')), ':h')
  :execute 'set runtimepath+='.s:mypath.'/vim/'
  call plug#begin(expand('~/.vim/plugged'))
  Plug 'luochen1990/rainbow'
  Plug 'prabirshrestha/vim-lsp'
  Plug 'mattn/vim-lsp-settings'
  Plug 'ionide/Ionide-vim'
  call plug#end()

  let g:rainbow_active = 1 "set to 0 if you want to enable it later via :RainbowToggle
  let g:lsp_diagnostics_enabled = 1

  function! s:on_lsp_buffer_enabled() abort
      setlocal omnifunc=lsp#complete
      nmap <buffer> gd <plug>(lsp-definition)
      nmap <buffer> gr <plug>(lsp-references)
      nmap <buffer> K <plug>(lsp-hover)
  endfunction

  augroup lsp_install
      au!
      " call s:on_lsp_buffer_enabled only for languages that has the server registered.
      autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
  augroup END


  " Enable LSP server during startup.
  let g:lsp_auto_enable = 1
  " Inlay hints.
  let g:lsp_inlay_hints_enabled = 1
  let g:lsp_inlay_hints_mode = { 'normal': ['always', '!curline'], 'insert': ['always', '!curline'],  }
  " Speeds up LSP
  let g:lsp_use_native_client = 1
endif

" You can embed vimscript in init.lua
" vim.cmd [[
"    " Example vimscript:
"    set expandtab
"    set smartindent
"    set breakindent
" 
"    nnoremap <Leader>tn :tabnext<CR>
" ]]

" Insert nvim lua in vimrc with:
" if has('nvim') | lua << EOF
"   <snippet>
" EOF
" endif

" Simple vimscripte to lua patterns
" let g:global_var = 1
" vim.g.global_var = 1
" 
" set     tabstop = 2
" vim.opt.tabstop = 2
" 
" nmap                 <leader>w    <cmd>wa<cr>
" vim.keymap.set('n', '<leader>w', '<cmd>wa<cr>')
" Also,
" local map = vim.keymap.set
" 
" map('n', '<leader>w', '<cmd>wa<cr>')
" // more mappings ...

