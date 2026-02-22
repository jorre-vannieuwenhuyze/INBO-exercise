\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{Vannieuwenhuyze_jorre_Article}

\LoadClass[11pt,a4paper]{article}

%%% --------------------------------------------------
%%% COLORS
%%% --------------------------------------------------

\RequirePackage{xcolor}
\definecolor{JorreGreen}{RGB}{0,131,121}
\colorlet{JorreLightgreen}{JorreGreen!50}
\colorlet{background}{JorreGreen!15!white}

%%% --------------------------------------------------
%%% FONT
%%% --------------------------------------------------

\RequirePackage[sfdefault]{ClearSans}
\renewcommand{\familydefault}{\sfdefault}

%%% --------------------------------------------------
%%% PAGE LAYOUT
%%% --------------------------------------------------

\RequirePackage[
hmargin=40mm,
top=25mm,
bottom=25mm
]{geometry}

\RequirePackage{parskip}
\widowpenalty=10000
\clubpenalty=10000
\sloppy


%%% --------------------------------------------------
%%% BACKGROUND SIDEBAR (ALL PAGES)
%%% --------------------------------------------------

\RequirePackage{tikz}
\RequirePackage{fancyhdr}
\pagestyle{fancy}
\fancyhf{}

\fancyhead{
	\begin{tikzpicture}[remember picture,overlay]
		\fill[background] (current page.south west)
		rectangle +(10mm,\paperheight);
	\end{tikzpicture}
}
\fancyfoot[R]{\thepage}

\renewcommand{\headrulewidth}{0pt}
\renewcommand{\footrulewidth}{0pt}

%%% --------------------------------------------------
%%% SECTION STYLING
%%% --------------------------------------------------

\RequirePackage{titlesec}

\titleformat{\section}
{\Large\bfseries\color{JorreGreen}}
{}
{0pt}
{}

\titleformat{\subsection}
{\large\bfseries\color{JorreGreen}}
{}
{0pt}
{}

\titleformat{\subsubsection}
{\bfseries\color{JorreLightgreen}}
{}
{0pt}
{}

%%% --------------------------------------------------
%%% TITLE BLOCK
%%% --------------------------------------------------

\renewcommand{\maketitle}{
	\begin{flushleft}
		{\Huge\bfseries\color{JorreGreen} \@title \par}
		\medskip
		{\large \@author \par}
		\medskip
		{\small \@date \par}
	\end{flushleft}
	\vspace{1cm}
}


%%% --------------------------------------------------
%%% FLOATS
%%% --------------------------------------------------

\usepackage{floatrow}
\floatsetup{font={sf,footnotesize},captionskip=.2\baselineskip,justification=raggedright,capposition=top}

\usepackage{caption}
\captionsetup{font={sf,footnotesize,it}
	,labelfont={bf}
	,justification=justified
	,singlelinecheck=off
	,labelsep=colon
} 


%%% --------------------------------------------------
%%% LINKS
%%% --------------------------------------------------

\RequirePackage[hidelinks]{hyperref}
