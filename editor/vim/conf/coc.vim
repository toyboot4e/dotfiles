" coc.vim
" TODO: refactor

" --------------------------------------------------------------------------------
" KEY MAPPINGS

""""""""""""""""""""""""""""""""""""""""
" copid from the official page: https://github.com/neoclide/coc.nvim
set shortmess+=c
set signcolumn=yes

" ----------------------------------------
" SNIPPETS
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm() : 
                                           \"\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" imap <C-l> <Plug>(coc-snippets-expand)
" imap <C-y> <Plug>(coc-snippets-expand-jump)
" vmap <C-j> <Plug>(coc-snippets-select)

" I use them for character conversion
" let g:coc_snippet_next = '<c-j>'
" let g:coc_snippet_prev = '<c-k>'
let g:coc_snippet_next = '<c-n>'
let g:coc_snippet_prev = '<c-p>'

" ----------------------------------------
" coc-prettier

command! -nargs=0 Prettier :CocCommand prettier.formatFile

" ----------------------------------------
" use TAB:
inoremap <silent><expr> <TAB>
      \ pumvisible() ? coc#_select_confirm() :
      \ coc#expandableOrJumpable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

let g:coc_snippet_next = '<tab>'

" ----------------------------------------
" COC/AUTOMATIC
" prefer <C-n> for tab for completion

" Highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" ----------------------------------------
" COC/MAPPINGS
" Tips: use Xmap for <Plug> commands (not Xnoremap)

" coc/jump
" nmap <silent> [g <Plug>(coc-diagnostic-prev)
" nmap <silent> ]g <Plug>(coc-diagnostic-next)

" goto
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> <C-w>gd :belowright vsplit<CR>:call CocAction('jumpDefinition')<CR>
" nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>
function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" ----------------------------------------
" coc/refactor
nnoremap <silent> <Leader>rF :Format<CR>
nmap <leader>rn <Plug>(coc-rename)
nmap <F2> <Plug>(coc-rename)

" <C-S-X> mapping not working (why)
" nmap <C-S-p>r <Plug>(coc-rename)

" ???: Create mappings for function text object, requires document symbols feature of languageserver.
xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)

" ----------------------------------------
" COC/COMMANDS
command! -nargs=0 Format :call CocAction('format')
" command! -nargs=? Fold   :call CocAction('fold', <f-args>)
command! -nargs=0 OR     :call CocAction('runCommand', 'editor.action.organizeImport')

" ----------------------------------------
" KEYMAPPINGS (NO FZF)

" " lists
" nnoremap <silent> <space>ld :<C-u>CocList diagnostics<cr>
" nnoremap <silent> <space>le :<C-u>CocList extensions<cr>
" nnoremap <silent> <space>lc :<C-u>CocList commands<cr>
" nnoremap <silent> <space>lo :<C-u>CocList outline<cr>
" nnoremap <silent> <space>ls :<C-u>CocList -I symbols<cr>
" " ???
" nnoremap <silent> <space>ln :<C-u>CocNext<CR>
" nnoremap <silent> <space>lp :<C-u>CocPrev<CR>
" " Resume latest coc list
" nnoremap <silent> <space>lp :<C-u>CocListResume<CR>

" ----------------------------------------
" KEYMAPPINGS (FZF)

nnoremap <silent> <space>la  :<C-u>CocFzfList diagnostics<CR>
nnoremap <silent> <space>lb  :<C-u>CocFzfList diagnostics --current-buf<CR>
nnoremap <silent> <space>lc  :<C-u>CocFzfList commands<CR>
nnoremap <silent> <space>le  :<C-u>CocFzfList extensions<CR>
nnoremap <silent> <space>ll  :<C-u>CocFzfList location<CR>
nnoremap <silent> <space>lo  :<C-u>CocFzfList outline<CR>
nnoremap <silent> <space>ls  :<C-u>CocFzfList symbols<CR>
nnoremap <silent> <space>lS  :<C-u>CocFzfList services<CR>
nnoremap <silent> <space>lp  :<C-u>CocFzfListResume<CR>

