\documentclass{article}\pagestyle{empty}
%include polycode.fmt

\usepackage{hyperref}
\usepackage{minted}
\usepackage{graphicx}
\usepackage{multicol}
\usepackage{datetime} \newcommand\timestamp{\twodigit{\year}-\twodigit{\month}-\twodigit{\day}}

\usepackage[letterpaper,top=1cm,bottom=1cm,left=2cm,right=2cm,head=0.5cm,foot=0.5cm,marginpar=0pt]{geometry}

\usepackage{fancyvrb}\VerbatimFootnotes % fancy \begin{Verbatim}[fontsize=\scriptsize]

\begin{document}\parindent=0pt
\phantom{.}\hfill\timestamp

\paragraph{Learning Haskell.}

For beginners there is a self study course at
\href{https://github.com/system-f/fp-course}{https://github.com/system-f/fp-course}.

If you want a gentle video class, you could try the class by Andres Löh,
\href{https://www.youtube.com/watch?v=3blAsQDT0u8&list=PLD8gywOEY4HauPWPfH0pJPIYUWqi0Gg10}{Introduction to Haskell}

If you want an even gentler video class, you could try the class by Tea Leaves,
\href{https://www.youtube.com/playlist?list=PLu6SHDdOToSe7ZOw-mR55j2GEjkNTQgrd}{Haskell for dilettantes},
which works through some of class materials from
Joachim Breitner's UPenn class CS194.

\paragraph{Installing Haskell.}
On linux or mac, install Haskell with ghcup, from
\begin{verbatim}
https://www.haskell.org/ghcup/install/
\end{verbatim}
% curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
Then you can upgrade and check the installation with
\begin{verbatim}
% ghcup upgrade
% ghcup tui
\end{verbatim}
In the tui display, one check means installed, two checks mean set for use.
Generally, I set the versions listed as `recommended' in that display.
See \href{https://www.haskell.org/ghcup/steps/}{https://www.haskell.org/ghcup/steps/} on
using external packages in {\tt ghci}, etc.

\smallskip
(On windows-11, I was able to install ghcup on windows subsystem ubuntu, just as in
a standard linux, after first installing:
build-essential findutils binutils curl gcc g++ libgmp-dev libc-dev libffi-dev make musl-dev ncurses-dev perl
tar xz-utils.) 

\paragraph{Optional: Literate Haskell}
To process literate Haskell files (*.lhs), install {\tt lhs2TeX} and {\tt unlit},
used to produce this documentation:
\begin{verbatim}
% stack install unlit
% stack install lhs2tex
\end{verbatim}
Then {\tt unlit} produces Haskell (*.hs) from literate Haskell (*.lhs).
For the {\tt installingHaskellEtc.lhs} file I am editing now,
\begin{verbatim}
% unlit -i installingHaskellEtc.lhs -o installingHaskellEtc.hs
\end{verbatim}

\medskip
For code documentation, line-by-line, {\tt lhs2tex}
produces a latex file for generating pdf, 
the pdf which you are presumably looking at now.
This file, {\tt installingHaskellEtc.pdf},
is produced and opened with these steps:
\begin{verbatim}
% lhs2tex -o installingHaskellEtc.tex installingHaskellEtc.lhs
% pdflatex -shell-escape installingHaskellEtc.tex
% open installingHaskellEtc.pdf
\end{verbatim}
Here in the pdf format,
the Haskell code which is now in {\tt installingHaskellEtc.hs} looks like this:
\begin{code}
main :: IO ()
main = putStrLn "Hello, world!"
\end{code}

\medskip
For code publication, the Haskell code in {\tt installingHaskellEtc.hs} can be much more colorful
and perhaps more beautiful with {\tt minted}:
\inputminted[frame=leftline,framesep=3mm]{haskell}{installingHaskellEtc.hs}

\medskip
The {\tt -shell-escape} flag for {\tt pdflatex} is required to use the {\tt minted} package.

\vfill\eject
\paragraph{Optional: emacs with haskell language server.} \ 
This is the development environment I prefer.
After installing a recent version of emacs,
inside emacs, I use package-install to install: ``haskell-mode``
Then I get an easy connection with language servers for syntax highlighting,  
code completion, etc. by adding the eglot initialization recommended here:

$\phantom{XXX}$
\href{https://haskell-language-server.readthedocs.io/en/latest/configuration.html\#emacs}{https://haskell-language-server.readthedocs.io/en/latest/configuration.html\#emacs}

\paragraph{Optional: tmux+neovim with haskell language server} \ 
Nvim also provides easy connection with language servers for syntax highlighting,
code completion, etc. 
Tmux allows you to split screen to run editor plus session
(plus anything else) all at once, with easy switching.
The combination is a very flexible and extensible
IDE, one that is actively used and developed recently.

\paragraph{Ubuntu nvim}\ 
Intro youtube vids, useful for those not already familiar with nvim and tmux:
\begin{itemize}
\item
\href{https://www.youtube.com/watch?v=Mtgo-nP_r8Y}{``Dreams of Code'' vid on nvim config and basics}
\item
\href{https://www.youtube.com/watch?v=DzNmUNvnB04}{``Dreams of Code'' vid on tmux config and basics}
\end{itemize}

On Ubuntu 24.04, I installed a nerdfont, ripgrep, tmux, tmux package manager (tpm), etc
\begin{Verbatim}[fontsize=\scriptsize]
% sudo apt install ripgrep
% sudo apt install nodejs npm
% sudo apt install luarocks
% sudo apt install tmux
% git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
% wget -P ~/.local/share/fonts https://github.com/ryanoasis/nerd-fonts/releases/latest/download/Meslo.zip
% cd ~/.local/share/fonts
% unzip Mezlo.zip
% rm Mezlo.zip
% sudo fc-cache -fv
% sudo npm i -g hls
% sudo npm i -g pyright
% ghcup install hls
% sudo luarocks install haskell-tools
% stack install haskell-dap ghci-dap haskell-debug-adapter
\end{Verbatim}
Then in a terminal window, in text settings, I selected the Meslo mono font.
Then following the instructions here:
\begin{center}
\href{https://nvchad.com/docs/quickstart/install/}{https://nvchad.com/docs/quickstart/install/}
\end{center}
In nvim, when running 
\begin{Verbatim}[fontsize=\scriptsize]
:MasonInstallAll,
\end{Verbatim}
you can scroll down to install haskell-language-server and pyright (and whatever else you want).
And in nvim, add syntax highlighting for whatever languages you want:
\begin{Verbatim}[fontsize=\scriptsize]
:TSInstall haskell
:TSInstall python
:TSInstall texlab
\end{Verbatim}
Then add these lines to {\tt .config/nvim/init.lua}:
\begin{Verbatim}[fontsize=\scriptsize]
require 'lspconfig'.pyright.setup{}
require 'lspconfig'.hls.setup{}
\end{Verbatim}
And in that same file, inside {\tt require("lazy").setup(\{ ...\})} add
\begin{Verbatim}[fontsize=\scriptsize]
  {
    'mrcjkb/haskell-tools.nvim',
     version = '^3', -- Recommended
     lazy = false, -- This plugin is already lazy
  },
\end{Verbatim}
Then create dir and file {\tt $\sim$/.config/nvim/after/ftplugin/haskell.lua} with contents:
\begin{Verbatim}[fontsize=\scriptsize]
-- ~/.config/nvim/after/ftplugin/haskell.lua
local ht = require('haskell-tools')
local bufnr = vim.api.nvim_get_current_buf()
local opts = { noremap = true, silent = true, buffer = bufnr, }
-- haskell-language-server relies heavily on codeLenses,
-- so auto-refresh (see advanced configuration) is enabled by default
vim.keymap.set('n', '<space>cl', vim.lsp.codelens.run, opts)
-- Hoogle search for the type signature of the definition under the cursor
vim.keymap.set('n', '<space>hs', ht.hoogle.hoogle_signature, opts)
-- Evaluate all code snippets
vim.keymap.set('n', '<space>ea', ht.lsp.buf_eval_all, opts)
-- Toggle a GHCi repl for the current package
vim.keymap.set('n', '<leader>rr', ht.repl.toggle, opts)
-- Toggle a GHCi repl for the current buffer
vim.keymap.set('n', '<leader>rf', function() ht.repl.toggle(vim.api.nvim_buf_get_name(0)) end, opts)
vim.keymap.set('n', '<leader>rq', ht.repl.quit, opts)
\end{Verbatim}

\paragraph{Mac nvim}\ 
On Mac, first,
\begin{itemize}

\item 
  At the time of this writing (May 2024)
  something is very wrong with the lua screen and color control for my default Mac terminal, so 
  I installed the kitty terminal, tmux, etc:
\begin{Verbatim}[fontsize=\scriptsize]
brew install kitty tmux tpm
\end{Verbatim}

  \item I installed all the homebrew nerd fonts:
\begin{Verbatim}[fontsize=\scriptsize]
brew tap homebrew/cask-fonts
brew search '/font-.*-nerd-font/' | awk '{ print $1 }' | xargs -I{} brew install --cask {} || true
\end{Verbatim}

  \item I installed ripgrep, haskell-language-server, debug tools
\begin{Verbatim}[fontsize=\scriptsize]
brew install ripgrep
ghcup install hls
stack install haskell-dap ghci-dap haskell-debug-adapter
\end{Verbatim}

  \item Opening a kitty terminal, in settings, I add
\begin{Verbatim}[fontsize=\scriptsize]
font_family      JetBrainsMono Nerd Font Mono
bold_font        JetBrainsMono Nerd Font Mono ExtraBold
bold_italic_font JetBrainsMono Nerd Font Mono ExtraBold Italic
\end{Verbatim}

\end{itemize}
Then, closing and reopening, \underline{in a kitty terminal}, 
I set up the NvChad configuration following the suggestions
used on the webpage mentioned above,
\href{https://nvchad.com/docs/quickstart/install/}{here}, and then
tell nvim to do syntax highlighting, add lines to {\tt init.lua}, create {\tt haskell.lua}
as described for Ubuntu, above.

\bigskip
Then start tmux in terminal; split screen into 2 panes; start nvim in
one of them; write, edit, save code in nvim;\break run code in the
other pane as it is developed.  An alternative that avoids learning
how tmux works is just to use nvim (with the NvChad configuration),
opening a terminal inside nvim with \textvisiblespace\,\verb!h!.

\bigskip
\begin{center}
  \includegraphics[width=7in]{figs/tmux-nvim-hs-session2.png}\\[30pt]
  %\includegraphics[width=5.5in]{figs/tmux-nvim-py-session2.png}
\end{center}

\vfill\eject

\def\tmuxLogo{\includegraphics[width=1.2in]{figs/Tmux_logo.svg.png}}
\def\neovimLogo{\includegraphics[width=1.6in]{figs/neovim-logo2.png}}
\def\nvchadLogo{\includegraphics[width=1.6in]{figs/nvchad-logo.png}}
\def\githubLogo{{\tabcolsep=1pt\tiny\begin{tabular}[c]\includegraphics[width=0.3in]{figs/github.png}\\config\end{tabular}}}
\def\vimLogo{\includegraphics[width=2.5in]{figs/vim_logo.png}}
\def\appleLogo{\includegraphics[scale=0.2]{Apple-logo.png}}
\def\commandKey{\includegraphics[scale=0.2]{figs/commandKeySmall.png}\thinspace}
\def\optionKey{\includegraphics[scale=0.5]{figs/menusym-option.png}\thinspace}

\pagestyle{empty}
 
\begin{multicols}{2}
\footnotesize

%\framebox{
%\begin{tabular}{@{}ll@{}}
%{\tt esc} & escape \\
%\return & return\\
%\commandKey  & command \\
%\optionKey  & option-alt \\
%$\Uparrow$ & shift (= cap if alphabetic) \\
%\textvisiblespace & space\\
%C- & control\\
%\end{tabular}
%\rule{0pt}{0cm}\rule{1cm}{0pt}
%%}

\begin{center}
  \tmuxLogo
  \href{https://github.com/dreamsofcode-io/tmux}{\tiny\begin{tabular}{c}\includegraphics[height=10pt]{figs/github.png}\\config\end{tabular}}
  \href{https://www.youtube.com/watch?v=DzNmUNvnB04}{\tiny\begin{tabular}{c}\includegraphics[height=16pt]{figs/youtube.png}\\config\end{tabular}}
  $\phantom{XXXX}$
\end{center}

{\framebox{
\begin{tabular}{ll}
\verb!<cntl>!-b & default {\tt<prefix>}\\
\verb!<cntl>!-\textvisiblespace& my {\tt<prefix>}\\
\end{tabular}
\rule{0pt}{0cm}\rule{3.5cm}{0pt}
}}

\smallskip
{\large\bf sessions}

\smallskip
\begin{tabular}{ll}
\verb!tmux new -s mysession1!  & new mysession1\\
\verb!tmux a mysession1!  & attach mysession1\\
\verb!tmux ls!  & list sessions\\
\verb!<cntl>!-\textvisiblespace\ \$ & rename session\\
\verb!<cntl>!-\textvisiblespace\ {\tt d}  & detach from session\\
\end{tabular}

\medskip
{\large\bf windows}

\smallskip
\begin{tabular}{ll}
\verb!<cntl>!-\textvisiblespace\ \verb!c!  & create new window\\
\verb!<cntl>!-\textvisiblespace\ \verb!p!  & previous window\\
\verb!<cntl>!-\textvisiblespace\ \verb!n!  & next window\\
\verb!<cntl>!-\textvisiblespace\ \verb!1...9!  & window by number\\
\verb!<cntl>!-\textvisiblespace\ \verb!&!  & close current window\\
\end{tabular}

\medskip
{\large\bf panes}

\smallskip
\begin{tabular}{ll}
\verb!<cntl>!-\textvisiblespace\ \verb!"!  & split horizontal\\
\verb!<cntl>!-\textvisiblespace\ \verb!%!  & split vertical\\
\verb!<cntl>!-\textvisiblespace\ \verb!z!  & toggle zoom\\
\verb!<cntl>!-\textvisiblespace\ $\leftarrow\,\uparrow\,\downarrow\,\rightarrow$  & switch pane (or mouse)\\
\verb!<cntl>!-\textvisiblespace\ \verb!h j k l! & switch pane (or mouse)\\
\verb!<cntl>!-\textvisiblespace\ {\tt!}  & convert pane to window\\
\verb!<cntl>!-\textvisiblespace\ \verb!x!  & close current pane\\
\end{tabular}

\begin{center}
\neovimLogo
\href{https://www.youtube.com/watch?v=c4OyfL5o7DU}{\tiny\begin{tabular}{c}\includegraphics[height=15pt]{figs/youtube.png}\\tutorial\end{tabular}}
\href{https://www.youtube.com/watch?v=Mtgo-nP_r8Y}{\tiny\begin{tabular}{c}\includegraphics[height=15pt]{figs/youtube.png}\\config\end{tabular}}
\href{https://github.com/NvChad/NvChad}{\tiny\begin{tabular}{c}\includegraphics[height=10pt]{figs/github.png}\\config\end{tabular}}
$\phantom{XXXXXX}$
\end{center}

{\large\bf motion}

\smallskip
\begin{tabular}{ll}
\verb!0!& zero moves cursor to beginning of line\\
\verb!$!& move cursor to end of line\\
\verb!M!& move cursor to middle line on screen\\
\verb!gg!& move to first line\\
\verb!G!& move to last line\\
\verb!#G!& goto line \#\\
\verb!#H!& goto \#th line in window\\
\end{tabular}

\medskip
{\large\bf general}

\smallskip
\begin{tabular}{ll}
\verb!:u! & undo last command\\
\verb!:undo! & undo last command\\
\verb!:redo! & redo last command\\
\verb!.! & repeat last command (in normal mode)\\
\verb!:sp! & split window (close w \verb!:q!)\\
\verb!:vsp! & split window vertically\\
\verb!:only! & make current window the only one\\
\verb!:new file! & split and open file\\
\end{tabular}

\medskip
{\large\bf save, write, quit}

\smallskip
\begin{tabular}{ll}
\verb!:q! \ \ \verb!:q!! & {\bf quit, quit quick}\\
\verb!:w file! & write to file\\
\verb!:wq!& save and quit\\
\verb!<cntl>!-\verb!s!& save\\
\end{tabular}

\medskip
{\large\bf search}

\smallskip
\begin{tabular}{ll}
\verb!/pattern!& search forward to pattern\\
\verb!?pattern!& search backward to pattern\\
\end{tabular}

\columnbreak
\medskip
{\large\bf cut and paste}

\smallskip
\begin{tabular}{ll}
\verb!yy!& yank 1 line\\
\verb!Y!& yank 1 line\\
\verb!Y$!& yank to end of line\\
\verb!#Y!& yank \# of lines\\
\verb!p! & put before cursor\\
\verb!P! & put after cursor\\
\end{tabular}

\medskip
{\large\bf edit}

\smallskip
\begin{tabular}{ll}
\verb!x! & delete symbol under cursor\\
\verb!X! & delete backwards from cursor\\
\verb!R! & replace to end of line\\
\verb!D! & delete to end of line\\
\verb!dd! & delete current line\\
\verb!i!\ \ \verb!a! & insert before, after cursor\\
\verb!I!\ \ \verb!A! & insert at beginning, end of line\\
\verb!esc! & {\bf escape insert mode}\\
\verb!J! & join next line w no space\\
\verb!gJ! & join next line w space\\
\verb!:s/old/new/! & replace next old w new\\
\verb!:s/old/new/g! & replace old w new in line\\
\verb!:%s/old/new/g! & replace old w new in file\\
\verb!:%s/old/new/gc! & replace with confirmations\\
\verb!~! & change case of char under cursor\\
\verb!vEU/u! & uppercase/lowercase word\\
\verb!vU/u! & uppercase/lowercase word\\
\end{tabular}

\medskip
{\large\bf insert}

\smallskip
\begin{tabular}{ll}
\verb!:r file!& insert file here\\
\end{tabular}

\begin{center}
\nvchadLogo
\href{https://github.com/NvChad/NvChad}{\tiny\begin{tabular}{c}\includegraphics[height=10pt]{figs/github.png}\\config\end{tabular}}
$\phantom{XXXXXXXX}$
\end{center}

\framebox{
\begin{tabular}{ll}
\textvisiblespace& my {\tt<prefix>}\\
\end{tabular}
\rule{0pt}{0cm}\rule{3.5cm}{0pt}
}

{\large\bf motion}

\smallskip
\begin{tabular}{ll}
\verb!<cntl>!-␣& move cursor to next word\\
\verb!<cntl>!-e& move screen forward 1 line\\
\verb!<cntl>!-y& move screen forward 1 line\\
\verb!<cntl>!-d& move forward $\frac{1}{2}$ screen\\
\verb!<cntl>!-u& move backward $\frac{1}{2}$ screen\\
\verb!<cntl>!-f& move forward $\frac{1}{2}$ screen\\
\verb!<cntl>!-b& move backward $\frac{1}{2}$ screen\\
\end{tabular}

\medskip
{\large\bf nvimtree}

\smallskip
\begin{tabular}{ll}
\verb!<cntl>!-n& toggle nvimtree \\
{\tt<return>} & in tree, open window on file under cursor\\
\verb!a! & in tree, add new file in current dir\\
\end{tabular}

\medskip
{\large\bf telescope}

\smallskip
\begin{tabular}{ll}
\textvisiblespace\,\verb!th! & select themes\\
\textvisiblespace\,\verb!fh! & find help\\
\textvisiblespace\,\verb!fb!& show buffers \\
\end{tabular}

\medskip
{\large\bf general}

\smallskip
\begin{tabular}{ll}
\textvisiblespace\,\verb!ch! & {\bf toggle cheat sheet}\\
\textvisiblespace\,\verb!n! & toggle line numbers\\
\textvisiblespace\,\verb!x! & close current buffer\\
\textvisiblespace\,\verb!h! & {\bf split and open terminal}\\
\end{tabular}

\medskip
{\large\bf haskell}

\smallskip
\begin{tabular}{ll}
\verb!:MasonInstallAll! & manage lsp's etc\\
\verb!:checkhealth lsp! & check lsp\\
\verb!:LspInfo! & check lsp\\
\textvisiblespace\,\verb!rr! & split and open ghci session\\
\textvisiblespace\,\verb!rq! & quit ghci session\\
\textvisiblespace\,\verb!ea! & evaluate code snippets\\
\textvisiblespace\,\verb!hs! & hoogle search sig under cursor\\
\textvisiblespace\,\verb!cl! & code lens refresh\\
\end{tabular}

\end{multicols}
\end{document}
