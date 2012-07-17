mercenary.vim
============

I'm not going to lie to you; mercenary.vim may very well be the worst
Mercurial wrapper of all time.

I forked [vim-fugitive](https://github.com/tpope/vim-fugitive) and am very 
slowly trying to get the most useful features of fugitive working for mercurial.  
The first step of this was `:%s/Fugitive/Mercenary` and `:%s/git/hg`. Beyond 
that, the going is slow, but it's slowly going. All the `:G____` commands have 
been replaced by `:HG____` commands.

This is a work in progress - most features are currently broken and will give 
you unexpected behaviour. Currently working is:

Vanilla `:HGblame`. Load up a file in a mercurial repository, then run 
`:HGblame` with no arguments.


Installation
------------

If you don't have a preferred installation method, I recommend
installing [pathogen.vim](https://github.com/tpope/vim-pathogen), and
then simply copy and paste:

    cd ~/.vim/bundle
    git clone git://github.com/phleet/vim-mercenary.git

Once help tags have been generated, you can view the manual with
`:help mercenary`.

License
-------
Fugitive is (c) Tim Pope.  Distributed under the same terms as Vim itself.
See `:help license`.

Mercurial modifications (c) Jamie Wong.
