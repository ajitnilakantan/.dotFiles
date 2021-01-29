source $VIMRUNTIME/defaults.vim

" == Store backup, undo, and swap files in temp directory
if has("win32")
  "Windows options here
  " :echom "Windows"
  set backupdir=c:\\.Backups\\\\
  set undodir=c:\\.Backups\\\\
  set directory=c:\\.Backups\\swapfiles\\\\
else
  if has("unix")
    let s:uname = system("uname")
    let s:unamev = system("uname -v")
    if s:unamev =~? ".*Microsoft.*"
        " Windows 10 / Linux
        set backupdir=/mnt/c/.Backups//
        set undodir=/mnt/c/.Backups//
        set directory=/mnt/c/.Backups/swapfiles//
        set directory=/mnt/c/.Backups//
        " :echom "Windows10 Linux"
    elseif s:uname == "Darwin\n"
        " Mac options here
        " :echom "MacOs"
        set mouse=v
        set backupdir=~/.Backups//,/tmp
        set undodir=~/.Backups//,/tmp
        set directory=~/.Backups/swapfiles//,/tmp
        set directory=~/.Backups//,/tmp
        " Automatically create .backup directory, writable by the group.
        if filewritable("~") && ! filewritable("~/.Backups")
          silent execute '!umask 002; mkdir ~/.Backups'
        endif
    endif
  endif
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
let s:mypath = fnamemodify(resolve(expand('<sfile>:p')), ':h')

"
" == Add vim support folder
"
:execute 'set runtimepath+='.s:mypath.'/vim/'

" Check if coming from ssh
let g:remoteSession = ($STY == "")

" == Plug Plugin
" :execute 'source '.s:mypath.'/vim/autoload/plug.vim'

" == Default font
if has("gui_running")
    set guifont=Consolas:h16
endif

" == Basic See: https://www.tutorialdocs.com/article/vim-configuration.html
set encoding=utf-8
set showmode                " Display the current mode
set mouse=a                 " Automatically enable mouse usage
set mousehide               " Hide the mouse cursor while typing
" filetype indent on
filetype plugin indent on   " Automatically detect file types.
syntax on                   " Syntax highlighting

if has('clipboard')
    if has('unnamedplus')  " When possible use + register for copy-paste
       set clipboard=unnamed,unnamedplus
    else         " On mac and Windows, use * register for copy-paste
        set clipboard=unnamed
    endif
endif

" == Searching
set number                      " Line numbers on
set showmatch                   " Show matching brackets/parenthesis
set incsearch                   " Find as you type search
set hlsearch                    " Highlight search terms
set matchtime=20
set cpoptions+=x
set winminheight=0              " Windows can be 0 line high
set ignorecase                  " Case insensitive search
set smartcase                   " Case sensitive when uc present
set scrolljump=5                " Lines to scroll when cursor leaves screen
set scrolloff=3                 " Minimum lines to keep above and below cursor

" == Formatting and Indentation
set backspace=indent,eol,start  " Backspace for dummies
set linespace=0                 " No extra spaces between rows
set nowrap                      " Do not wrap long lines
set autoindent                  " Indent at the same level of the previous line
set shiftwidth=4                " Use indents of 4 spaces
set expandtab                   " Tabs are spaces, not tabs
set tabstop=4                   " An indentation every four columns
set softtabstop=4               " Let backspace delete indent
set nojoinspaces                " Prevents inserting two spaces after punctuation on a join (J)
set splitright                  " Puts new vsplit windows to the right of the current
set splitbelow                  " Puts new split windows to the bottom of the current
"set matchpairs+=<:>             " Match, to be used with %
set pastetoggle=<F12>           " pastetoggle (sane indentation on pastes)
set foldenable                  " Auto fold code
set list
set listchars=tab:›\ ,trail:•,extends:#,nbsp:. " Highlight problematic whitespace

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

if &term == 'xterm-256color'
    " More pleasant color scheme when you ssh to macos from windows
    " call plug#begin(expand('~/.vim/plugged'))
    " Plug 'lifepillar/vim-solarized8'
    " call plug#end() " Run :PlugInstall to install
    if g:remoteSession
        colorscheme slate
    else
        colorscheme solarized8
    endif
else
    colorscheme darkblue
endif

hi User1 guifg=#ffdad8  guibg=#880c0e
hi User2 guifg=#000000  guibg=#F4905C
hi User3 guifg=#292b00  guibg=#f4f597
hi User4 guifg=#112605  guibg=#aefe7B
hi User5 guifg=#051d00  guibg=#7dcc7d
hi User7 guifg=#ffffff  guibg=#880c0e gui=bold
hi User8 guifg=#ffffff  guibg=#5b7fbb
hi User9 guifg=#ffffff  guibg=#810085
hi User0 guifg=#ffffff  guibg=#094afe

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
    set statusline+=%7*\[%n]                                  "buffernr
    set statusline+=%1*\ %<%F\                                "File+path
    set statusline+=%2*\ %y\                                  "FileType
    set statusline+=%3*\ %{''.(&fenc!=''?&fenc:&enc).''}      "Encoding
    set statusline+=%3*\ %{(&bomb?\",BOM\":\"\")}\            "Encoding2
    set statusline+=%4*\ %{&ff}\                              "FileFormat (dos/unix..) 
    set statusline+=%5*\ %{&spelllang}\ "Spellanguage
    set statusline+=%8*\ %=\ row:%l/%L\ (%03p%%)\             "Rownumber/total (%)
    set statusline+=%9*\ col:%03c\                            "Colnr
    set statusline+=%0*\ \ %m%r%w\ %P\ \                      "Modified? Readonly? Top/bot.
endif

" == Misc shortcuts
" Shift-Tab to Outdent
" for command mode
nnoremap <S-Tab> <<
" for insert mode
inoremap <S-Tab> <C-d>
" Show character count g-<C-G>
nnoremap ;gg           G$g<C-G>''

" Clear search highlight
" This unsets the "last search pattern" register by hitting return. Keep
" comment on separate line
nnoremap <silent> <CR> :noh<CR><CR>


function! SynStack ()
    for i1 in synstack(line("."), col("."))
        let i2 = synIDtrans(i1)
        let n1 = synIDattr(i1, "name")
        let n2 = synIDattr(i2, "name")
        echo n1 "->" n2
    endfor
endfunction
map gm :call SynStack()<CR>


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
nnoremap <C-Left>  :tabprevious<CR>
nnoremap <C-Right> :tabnext<CR>

" == Rainbow parentheses
""" Run :PlugInstall
call plug#begin(expand('~/.vim/plugged'))
Plug 'luochen1990/rainbow'
call plug#end()
let g:rainbow_active = 1 "set to 0 if you want to enable it later via :RainbowToggle

