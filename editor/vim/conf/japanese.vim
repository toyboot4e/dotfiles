" japanese.vim

" typewriter scroll (always place the cursor on center)
" set scrolloff=999

" --------------------------------------------------------------------------------
" OPTIONS

let g:surround_{char2nr("「")} = "「 \r 」"
let g:surround_{char2nr("」")} = "「\r」"
let g:surround_{char2nr("『")} = "『 \r 』"
let g:surround_{char2nr("』")} = "『\r』"
let g:surround_{char2nr("【")} = "【 \r 】"
let g:surround_{char2nr("】")} = "【\r】"
let g:surround_{char2nr("（")} = "（ \r ）"
let g:surround_{char2nr("）")} = "（\r）"
let g:surround_{char2nr("＜")} = "＜ \r ＞"
let g:surround_{char2nr("＞")} = "＜\r＞"
let g:surround_{char2nr("《")} = "《 \r 》"
let g:surround_{char2nr("》")} = "《\r》"
let g:surround_{char2nr("｛")} = "｛ \r ｝"
let g:surround_{char2nr("｝")} = "｛\r｝"

set matchpairs& matchpairs+=<:>
set matchpairs+=「:」,『:』,（:）,【:】,《:》,〈:〉,［:］,‘:’,“:”

" When joining, do not add white spaces for multi bytes characters
set formatoptions+=mMj

" force fixed length for symbols
" set ambiwidth=double

" --------------------------------------------------------------------------------
" JUMP

function! s:MapFTR(key, char)
  for cmd in ['f', 'F', 't', 'T', 'r']
    execute 'noremap <silent> ' . cmd . a:key . ' ' . cmd . a:char
  endfor
endfunction

function! s:UnmapFTR(key)
  for cmd in ['f', 'F', 't', 'T', 'r']
    execute 'nunmap <silent> ' . cmd . a:key
  endfor
endfunction

" REMARK: We have to type them within `timeoutlen` ??
call <SID>MapFTR('<C-j>', '、')
call <SID>MapFTR('<C-l>', '。')

nnoremap [s F、
nnoremap ]s f、
nnoremap [d F。
nnoremap ]d f。

nnoremap [S T、
nnoremap ]S t、
nnoremap [D T。
nnoremap ]D t。

