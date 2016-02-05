If you use Emacs, you're likely to use several packages that help
manipulate text.  Whether it's evil, lispy, paredit, or
multiple-cursors, I think we can all agree that third-party packages
are essential to the Emacs experience.

Packages define several of their own keybindings to use in
particular modes.  When combined with the $NUBMEROFBINDINGS default
bindings that Emacs ships with, one can end up with 100s of different
shortcuts for manipulating text.  Memorizing all of them is
unreasonable, and learning from the documentation is slow.

Shorty is here to solve that.

Shorty is a tool for both Emacs users and Emacs package authors.  It
allows authors to easily build playlists of shortcut demos and allows
users to play them back in sequeunce.

Learning by doing is arguably the best way to learn.  Since, shorty
demos are executed live inside an Emacs buffer, you can hop in once
the demo's done and the try it out for yourself. Learning is a breeze
becase every demo comes with a key logger that shows the keypresses
executed for a particular demo.  No more fumbling with an emacs
window and documentation window to learn a new tool.  With Shorty, you
just press play.

Demos are stored in a .shorty file.  For all intents and purposes,
a shorty file is org file. The only difference is that when a 
shorty file is opened, 'shorty-mode' is enabled in addition to
org-mode.

Shorty mode enables special commands for building and viewing demos.
*Demos are built and viewed from within this file.*  Lets go over the 
structure of a shorty file.

Structure
