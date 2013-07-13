" Vim syntax file
" Language:	Markdown
" Maintainer:	Ben Williams <benw@plasticboy.com>
" URL:		http://plasticboy.com/markdown-vim-mode/
" Version:	9
" Last Change:  2009 May 18 
" Remark:	Uses HTML syntax file
" Remark:	I don't do anything with angle brackets (<>) because that would too easily
"		easily conflict with HTML syntax
" TODO: 	Handle stuff contained within stuff (e.g. headings within blockquotes)


" Read the HTML syntax to start with
if version < 600
  so <sfile>:p:h/html.vim
else
  runtime! syntax/html.vim
  unlet b:current_syntax
endif

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" don't use standard HiLink, it will not work with included syntax files
if version < 508
  command! -nargs=+ HtmlHiLink hi link <args>
else
  command! -nargs=+ HtmlHiLink hi def link <args>
endif

syn spell toplevel
syn case ignore
syn sync linebreaks=1

"additions to HTML groups
syn region htmlBold     start=/\\\@<!\(^\|\A\)\@=\*\@<!\*\*\*\@!/     end=/\\\@<!\*\@<!\*\*\*\@!\($\|\A\)\@=/   contains=@Spell,htmlItalic
syn region htmlItalic   start=/\\\@<!\(^\|\A\)\@=\*\@<!\*\*\@!/       end=/\\\@<!\*\@<!\*\*\@!\($\|\A\)\@=/      contains=htmlBold,@Spell
syn region htmlBold     start=/\\\@<!\(^\|\A\)\@=_\@<!___\@!/         end=/\\\@<!_\@<!___\@!\($\|\A\)\@=/       contains=htmlItalic,@Spell
syn region htmlItalic   start=/\\\@<!\(^\|\A\)\@=_\@<!__\@!/          end=/\\\@<!_\@<!__\@!\($\|\A\)\@=/        contains=htmlBold,@Spell

" [link](URL) | [link][id] | [link][]
syn region markdownLink matchgroup=markdownDelimiter      start="\!\?\[" end="\]\ze\s*[[(]" contains=@Spell nextgroup=markdownURL,markdownID skipwhite oneline
syn region markdownID matchgroup=markdownDelimiter        start="\["    end="\]" contained
syn region markdownURL matchgroup=markdownDelimiter       start="("     end=")"  contained

" Link definitions: [id]: URL (Optional Title)
" TODO handle automatic links without colliding with htmlTag (<URL>)
syn region markdownLinkDef matchgroup=markdownDelimiter   start="^ \{,3}\zs\[" end="]:" oneline nextgroup=markdownLinkDefTarget skipwhite
syn region markdownLinkDefTarget start="<\?\zs\S" excludenl end="\ze[>[:space:]\n]"   contained nextgroup=markdownLinkTitle,markdownLinkDef skipwhite skipnl oneline
syn region markdownLinkTitle matchgroup=markdownDelimiter start=+"+     end=+"+  contained
syn region markdownLinkTitle matchgroup=markdownDelimiter start=+'+     end=+'+  contained
syn region markdownLinkTitle matchgroup=markdownDelimiter start=+(+     end=+)+  contained

"define Markdown groups
syn match  markdownLineContinue ".$" contained
syn match  markdownRule      /^\s*\*\s\{0,1}\*\s\{0,1}\*$/
syn match  markdownRule      /^\s*-\s\{0,1}-\s\{0,1}-$/
syn match  markdownRule      /^\s*_\s\{0,1}_\s\{0,1}_$/
syn match  markdownRule      /^\s*-\{3,}$/
syn match  markdownRule      /^\s*\*\{3,5}$/
syn match  markdownListItem  "^\s*[-*+]\s\+"
syn match  markdownListItem  "^\s*\d\+\.\s\+"
syn match  markdownCode      /^\s*\n\(\(\s\{4,}[^ ]\|\t\+[^\t]\).*\n\)\+/
syn match  markdownLineBreak /  \+$/
syn region markdownCode      start=/\\\@<!`/                   end=/\\\@<!`/
syn region markdownCode      start=/\s*``[^`]*/          end=/[^`]*``\s*/
syn region markdownBlockquote start=/^\s*>/              end=/$/                 contains=markdownLineBreak,markdownLineContinue,@Spell
syn region markdownCode      start="<pre[^>]*>"         end="</pre>"
syn region markdownCode      start="<code[^>]*>"        end="</code>"

"HTML headings
syn region htmlH1       start="^\s*#"                   end="\($\|#\+\)" contains=@Spell
syn region htmlH2       start="^\s*##"                  end="\($\|#\+\)" contains=@Spell
syn region htmlH3       start="^\s*###"                 end="\($\|#\+\)" contains=@Spell
syn region htmlH4       start="^\s*####"                end="\($\|#\+\)" contains=@Spell
syn region htmlH5       start="^\s*#####"               end="\($\|#\+\)" contains=@Spell
syn region htmlH6       start="^\s*######"              end="\($\|#\+\)" contains=@Spell
syn match  htmlH1       /^.\+\n=\+$/ contains=@Spell
syn match  htmlH2       /^.\+\n-\+$/ contains=@Spell

"highlighting for Markdown groups
HtmlHiLink markdownString	    String
HtmlHiLink markdownCode          String
HtmlHiLink markdownBlockquote    Comment
HtmlHiLink markdownLineContinue  Comment
HtmlHiLink markdownListItem      Identifier
HtmlHiLink markdownRule          Identifier
HtmlHiLink markdownLineBreak     Todo
HtmlHiLink markdownLink          htmlLink
HtmlHiLink markdownURL           htmlString
HtmlHiLink markdownID            Identifier
HtmlHiLink markdownLinkDef       markdownID
HtmlHiLink markdownLinkDefTarget markdownURL
HtmlHiLink markdownLinkTitle     htmlString

HtmlHiLink markdownDelimiter     Delimiter

let b:current_syntax = "markdown"

delcommand HtmlHiLink
" vim: ts=8
