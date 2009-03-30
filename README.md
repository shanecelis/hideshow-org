hideshow-org.el
===============

hideshow-org.el provides an org-mode like interface to the
hideshow.el file.

org-mode provides an elegant means of interacting with outlines
that one can toggle with the TAB key and Shift TAB.
hideshow-org.el is my attempt to bring the org-mode like hiding and
showing to code.

## Download

    $ git clone git://github.com/secelis/hideshow-org.git

## Installation

    (add-to-list 'load-path "/path/to/hideshow-org-directory")
    (require 'hideshow-org)

## Keymaps

I set this as my global key.

    (global-set-key "\C-ch" 'hs-org/minor-mode)

Here are the keys in the minor mode.

    TAB       -- execute normal TAB command, if point doesn't move, try to
                 toggle the visibility of the block.
    <S-tab>   -- execute normal <S-tab command, if point doesn't move, try to
                 toggle the visibility of all the blocks.

## Notes

The hardest part was trying to figure out when TAB should behave
normally: for code that usually means some variant of *-ident-line
(and is incidentally one of the reason I love emacs); for outlines
in org-mode that means toggling the visibility.  So the solution I
came up with is this.  If TAB doesn't change the point, then we try
to hide or show the block.  And we do a similar thing for Shift
TAB.  Hopefully, this will be sufficient such that
hs-org/minor-mode does not get in the way of anyone's normal
programming habits.

Many thanks to the developers of hideshow.el.  Thanks to
yasnippets.el for showing me how one could piggyback on an already
bound key.
