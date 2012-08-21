mercenary.vim
============

I'm not going to lie to you; mercenary.vim may very well be the worst
Mercurial wrapper of all time.

Installation
------------

If you don't have a preferred installation method, I recommend
installing [vundle](https://github.com/gmarik/vundle) and adding

    Bundle 'phleet/vim-mercenary'

to your `.vimrc` then running `:BundleInstall`.

If you prefer [pathogen.vim](https://github.com/tpope/vim-pathogen), after 
installing it, simply copy and paste:

    cd ~/.vim/bundle
    git clone git://github.com/phleet/vim-mercenary.git

Once help tags have been generated, you can view the manual with
`:help mercenary`.

License
-------
Mercenary is (c) Jamie Wong.

Distributed under the same terms as Vim itself.  See `:help license`.

Heavily inspired by vim-fugitive by Time Pope: 
https://github.com/tpope/vim-fugitive

This started out as a fork of fugitive, but I eventually discovered that the 
differences between git and mercurial while minor in functionality are vast in 
implementation. So I started from scratch, using fugitive's code as a reference 
but re-implementing everything to be less of a monkey patch for mercurial.
