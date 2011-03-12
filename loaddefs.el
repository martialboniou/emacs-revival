;;
;;;### (autoloads (run-at-time) "FSF-timer" "mailcrypt-3.5.9/FSF-timer.el"
;;;;;;  (19352 62013))
;;; Generated autoloads from mailcrypt-3.5.9/FSF-timer.el

(autoload 'run-at-time "FSF-timer" "\
Run a function at a time, and optionally on a regular interval.
Arguments are TIME, REPEAT, FUNCTION &rest ARGS.
TIME, a string, can be specified absolutely or relative to now.
TIME can also be an integer, a number of seconds.
REPEAT, an integer number of seconds, is the interval on which to repeat
the call to the function.  If REPEAT is nil or 0, call it just once.

Absolute times may be specified in a wide variety of formats;
Something of the form `HOUR:MIN:SEC TIMEZONE MONTH/DAY/YEAR', where
all fields are numbers, works; the format used by the Unix `date'
command works too.

Relative times may be specified as a series of numbers followed by units:
  1 min         	denotes one minute from now.
  min			does too.
  1 min 5 sec		denotes 65 seconds from now.
  1 min 2 sec 3 hour 4 day 5 week 6 fortnight 7 month 8 year
			denotes the sum of all the given durations from now.

\(fn TIME REPEAT FUNCTION &rest ARGS)" t nil)

;;;***
;;; loaddefs.el --- Auto-generated CEDET autoloads

;;;### (autoloads (anything-other-buffer anything-at-point anything)
;;;;;;  "anything" "anything.el" (19814 14041))
;;; Generated autoloads from anything.el

(autoload 'anything "anything" "\
Select anything. In Lisp program, some optional arguments can be used.

PLIST is a list like (:key1 val1 :key2 val2 ...) or
 (&optional sources input prompt resume preselect buffer keymap).

Basic keywords are the following:

- :sources

  Temporary value of `anything-sources'.  It also accepts a
  symbol, interpreted as a variable of an anything source.  It
  also accepts an alist representing an anything source, which is
  detected by (assq 'name ANY-SOURCES)

- :input

  Temporary value of `anything-pattern', ie. initial input of minibuffer.

- :prompt

  Prompt other than \"pattern: \".

- :resume

  If t, Resurrect previously instance of `anything'. Skip the initialization.
  If 'noresume, this instance of `anything' cannot be resumed.

- :preselect

  Initially selected candidate. Specified by exact candidate or a regexp.
  Note that it is not working with delayed sources.

- :buffer

  `anything-buffer' instead of *anything*.

- :keymap

  `anything-map' for current `anything' session.


Of course, conventional arguments are supported, the two are same.

 (anything :sources sources :input input :prompt prompt :resume resume
           :preselect preselect :buffer buffer :keymap keymap)
 (anything sources input prompt resume preselect buffer keymap)
           

Other keywords are interpreted as local variables of this anything session.
The `anything-' prefix can be omitted. For example,

 (anything :sources 'anything-c-source-buffers
           :buffer \"*buffers*\" :candidate-number-limit 10)

means starting anything session with `anything-c-source-buffers'
source in *buffers* buffer and set
`anything-candidate-number-limit' to 10 as session local variable. 

\(fn &rest PLIST)" t nil)

(autoload 'anything-at-point "anything" "\
Same as `anything' except when C-u is pressed, the initial input is the symbol at point.

\(fn &optional ANY-SOURCES ANY-INPUT ANY-PROMPT ANY-RESUME ANY-PRESELECT ANY-BUFFER)" t nil)

(autoload 'anything-other-buffer "anything" "\
Simplified interface of `anything' with other `anything-buffer'

\(fn ANY-SOURCES ANY-BUFFER)" nil nil)

;;;***

;;;### (autoloads (ascii-off ascii-on ascii-display ascii-customize)
;;;;;;  "ascii" "ascii.el" (19828 49560))
;;; Generated autoloads from ascii.el

(autoload 'ascii-customize "ascii" "\
Customize ASCII options.

\(fn)" t nil)

(autoload 'ascii-display "ascii" "\
Toggle ASCII code display.

If ARG is null, toggle ASCII code display.
If ARG is a number and is greater than zero, turn on display; otherwise, turn
off display.
If ARG is anything else, turn on display.

\(fn &optional ARG)" t nil)

(autoload 'ascii-on "ascii" "\
Turn on ASCII code display.

\(fn)" t nil)

(autoload 'ascii-off "ascii" "\
Turn off ASCII code display.

\(fn)" t nil)

;;;***

;;;### (autoloads (autoconf-parameters-for-macro) "autoconf-edit"
;;;;;;  "cedet/ede/autoconf-edit.el" (19530 61660))
;;; Generated autoloads from cedet/ede/autoconf-edit.el

(autoload 'autoconf-parameters-for-macro "autoconf-edit" "\
Retrieve the parameters to MACRO.
Returns a list of the arguments passed into MACRO as strings.

\(fn MACRO &optional IGNORE-BOL IGNORE-CASE)" nil nil)

;;;***

;;;### (autoloads (switch-to-buffer autofit-frames-flag) "autofit-frame"
;;;;;;  "autofit-frame.el" (19826 9540))
;;; Generated autoloads from autofit-frame.el

(defvar autofit-frames-flag t "\
*Non-nil means automatically resize one-window frames to fit buffer.")

(custom-autoload 'autofit-frames-flag "autofit-frame" t)

(autoload 'switch-to-buffer "autofit-frame" "\
Select buffer BUFFER in current window, unless the window is dedicated.
If current window is dedicated (`window-dedicated-p'), then another window
is used.

BUFFER may be a buffer, a string (a buffer name), or nil.  If BUFFER
is a string that does not identify an existing buffer, then a new
buffer with that name is created.  If BUFFER is nil, then function
`other-buffer' is used to choose a buffer.

Optional second arg NORECORD non-nil means do not put BUFFER at the
front of the list of recently selected buffers.

The buffer switched to is returned.

*WARNING*: This is NOT the way to work on another buffer temporarily
within a Lisp program!  Use `set-buffer' instead, to avoid messing
with correspondences between windows and buffers.

Resizes frame to fit sole window if `autofit-frames-flag'
\(unless BUFFER is already the `current-buffer').

\(fn BUFFER &optional NORECORD)" t nil)

;;;***

;;;### (autoloads (bbdb-insinuate-message bbdb-initialize bbdb-multiple-buffers
;;;;;;  bbdb-submit-bug-report) "bbdb" "bbdb/lisp/bbdb.el" (19383
;;;;;;  20742))
;;; Generated autoloads from bbdb/lisp/bbdb.el

(autoload 'bbdb-submit-bug-report "bbdb" "\
Submit a bug report, with pertinent information to the BBDB info list.

\(fn)" t nil)

(defvar bbdb-multiple-buffers nil "\
When non-nil we create a new buffer of every buffer causing pop-ups.
You can also set this to a function returning a buffer name.")

(custom-autoload 'bbdb-multiple-buffers "bbdb" t)

(autoload 'bbdb-initialize "bbdb" "\
*Initialize the BBDB.  One or more of the following symbols can be
passed as arguments to initiate the appropriate insinuations.

 Initialization of mail/news readers:

   gnus       Initialize BBDB support for the gnus mail/news reader
              version 3.15 or newer.  If you pass the `gnus' symbol,
              you should probably also pass the `message' symbol.
   mh-e       Initialize BBDB support for the MH-E mail reader.
   rmail      Initialize BBDB support for the RMAIL mail reader.
   sendmail   Initialize BBDB support for sendmail (M-x mail).
   vm         Initialize BBDB support for the VM mail reader.
              NOTE: For the VM insinuation to work properly, you must
              either call `bbdb-initialize' with the `vm' symbol from
              within your VM initialization file (\"~/.vm\") or you
              must call `bbdb-insinuate-vm' manually from within your
              VM initialization file.

 Initialization of miscellaneous package:

   message    Initialize BBDB support for Message mode.
   reportmail Initialize BBDB support for the Reportmail mail
              notification package.
   sc or      Initialize BBDB support for the Supercite message
   supercite  citation package.
   w3         Initialize BBDB support for Web browsers.

\(fn &rest TO-INSINUATE)" nil nil)

(autoload 'bbdb-insinuate-message "bbdb" "\
Call this function to hook BBDB into `message-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (bbdb-include-anniversaries bbdb-anniversaries
;;;;;;  bbdb-utilities-anniversaries) "bbdb-anniv" "bbdb/bits/bbdb-anniv.el"
;;;;;;  (15006 27895))
;;; Generated autoloads from bbdb/bits/bbdb-anniv.el

(let ((loads (get 'bbdb-utilities-anniversaries 'custom-loads))) (if (member '"bbdb-anniv" loads) nil (put 'bbdb-utilities-anniversaries 'custom-loads (cons '"bbdb-anniv" loads))))

(defvar bbdb-anniversaries nil "\
Should BBDB anniversaries be included when the diary is displayed (fancy)?
You must modify via \\[customize] for this variable to have an effect.")

(custom-autoload 'bbdb-anniversaries "bbdb-anniv" nil)

(autoload 'bbdb-include-anniversaries "bbdb-anniv" "\
Not documented

\(fn)" nil nil)

;;;***

;;;### (autoloads (bbdb-get-only-first-address-p bbdb-get-addresses-headers
;;;;;;  bbdb-update-records bbdb-update-records-mode bbdb-help bbdb-info
;;;;;;  bbdb-creation-no-change bbdb-creation-newer bbdb-creation-older
;;;;;;  bbdb-timestamp-newer bbdb-timestamp-older bbdb-finger bbdb-dial
;;;;;;  bbdb-add-or-remove-mail-alias bbdb-define-all-aliases bbdb-yank
;;;;;;  bbdb-complete-name bbdb-read-addresses-with-completion bbdb-completion-predicate
;;;;;;  bbdb-completion-check-record bbdb-show-all-recipients bbdb-send-mail
;;;;;;  bbdb-dwim-net-address bbdb-sort-addresses bbdb-sort-phones
;;;;;;  bbdb-sort-notes bbdb-refile-record bbdb-omit-record bbdb-display-record-with-layout
;;;;;;  bbdb-display-record-completely bbdb-display-all-records-completely
;;;;;;  bbdb-toggle-records-display-layout bbdb-toggle-all-records-display-layout
;;;;;;  bbdb-delete-current-record bbdb-delete-current-field-or-record
;;;;;;  bbdb-transpose-fields bbdb-record-edit-property bbdb-record-edit-notes
;;;;;;  bbdb-edit-current-field bbdb-insert-new-field bbdb-append-records
;;;;;;  bbdb-append-records-p bbdb-apply-next-command-to-all-records
;;;;;;  bbdb-create bbdb-redisplay-records bbdb-changed bbdb-notes
;;;;;;  bbdb-net bbdb-company bbdb-name bbdb bbdb-search-invert-set)
;;;;;;  "bbdb-com" "bbdb/lisp/bbdb-com.el" (19383 20957))
;;; Generated autoloads from bbdb/lisp/bbdb-com.el

(autoload 'bbdb-search-invert-set "bbdb-com" "\
Typing \\<bbdb-mode-map>\\[bbdb-search-invert-set] inverts the meaning of the next search command.
Sets `bbdb-search-invert' to t.
You will have to call this function again, if you want to
do repeated inverted searches.

\(fn)" t nil)

(autoload 'bbdb "bbdb-com" "\
Display all entries in the BBDB matching the regexp STRING
in either the name(s), company, network address, or notes.

\(fn STRING ELIDEP)" t nil)

(autoload 'bbdb-name "bbdb-com" "\
Display all entries in the BBDB matching the regexp STRING in the name
\(or ``alternate'' names).

\(fn STRING ELIDEP)" t nil)

(autoload 'bbdb-company "bbdb-com" "\
Display all entries in BBDB matching STRING in the company field.

\(fn STRING ELIDEP)" t nil)

(autoload 'bbdb-net "bbdb-com" "\
Display all entries in BBDB matching regexp STRING in the network address.

\(fn STRING ELIDEP)" t nil)

(autoload 'bbdb-notes "bbdb-com" "\
Display all entries in BBDB matching STRING in the named notes field.

\(fn WHICH STRING ELIDEP)" t nil)

(autoload 'bbdb-changed "bbdb-com" "\
Display all entries in the bbdb database which have been changed since
the database was last saved.

\(fn ELIDEP)" t nil)

(autoload 'bbdb-redisplay-records "bbdb-com" "\
Regrinds the contents of the *BBDB* buffer, without scrolling.
If possible, you should call `bbdb-redisplay-one-record' instead.

\(fn)" nil nil)

(autoload 'bbdb-create "bbdb-com" "\
Add a new entry to the bbdb database ; prompts for all relevant info
using the echo area, inserts the new record in the db, sorted alphabetically,
and offers to save the db file.  DO NOT call this from a program.  Call
bbdb-create-internal instead.

\(fn RECORD)" t nil)

(autoload 'bbdb-apply-next-command-to-all-records "bbdb-com" "\
Typing \\<bbdb-mode-map>\\[bbdb-apply-next-command-to-all-records] in the *BBDB* buffer makes the next command operate on all
of the records currently displayed.  (Note that this only works for
certain commands.)

\(fn)" t nil)

(autoload 'bbdb-append-records-p "bbdb-com" "\
Not documented

\(fn)" nil nil)

(autoload 'bbdb-append-records "bbdb-com" "\
Typing \\<bbdb-mode-map>\\[bbdb-append-records] in the *BBDB* buffer makes the next search/display command to append
new records to those in the *BBDB* buffer.

With an prefix arg (C-u) toggle between always append and no append.
With an prefix arg that is a positive number append will be enabled for that
many times.
With any other argument append will be enabled once.

\(fn ARG)" t nil)

(autoload 'bbdb-insert-new-field "bbdb-com" "\
Add a new field to the current record; the field type and contents
are prompted for if not supplied.

If you are inserting a new phone-number field, you can control whether
it is a north american or european phone number by providing a prefix
argument.  A prefix arg of ^U means it's to be a euronumber, and any
other prefix arg means it's to be a a structured north american number.
Otherwise, which style is used is controlled by the variable
`bbdb-north-american-phone-numbers-p'.

If you are inserting a new net address, you can have BBDB append a
default domain to any net address that does not contain one.  Set
`bbdb-default-domain' to a string such as \"mycompany.com\" (or,
depending on your environment, (getenv \"DOMAINNAME\")), and
\"@mycompany.com\" will be appended to an address that is entered as
just a username.  A prefix arg of ^U (or a `bbdb-default-domain'
value of \"\", the default) means do not alter the address.

\(fn RECORD NAME CONTENTS)" t nil)

(autoload 'bbdb-edit-current-field "bbdb-com" "\
Edit the contents of the Insidious Big Brother Database field displayed on
the current line (this is only meaningful in the \"*BBDB*\" buffer.)   If the
cursor is in the middle of a multi-line field, such as an address or comments
section, then the entire field is edited, not just the current line.

\(fn)" t nil)

(autoload 'bbdb-record-edit-notes "bbdb-com" "\
Not documented

\(fn BBDB-RECORD &optional REGRIND)" t nil)

(autoload 'bbdb-record-edit-property "bbdb-com" "\
Not documented

\(fn BBDB-RECORD &optional PROP REGRIND)" t nil)

(autoload 'bbdb-transpose-fields "bbdb-com" "\
This is like the `transpose-lines' command, but it is for BBDB fields.
If the cursor is on a field of a BBDB record, that field and the previous
field will be transposed.

With argument ARG, takes previous line and moves it past ARG fields.
With argument 0, interchanges field point is in with field mark is in.

Both fields must be in the same record, and must be of the same basic type
\(that is, you can use this command to change the order in which phone-number
fields are listed, but you can't use it to make an address appear before a
phone number; the order of field types is fixed.)

\(fn &optional ARG)" t nil)

(autoload 'bbdb-delete-current-field-or-record "bbdb-com" "\
Delete the line which the cursor is on; actually, delete the field which
that line represents from the database.  If the cursor is on the first line
of a database entry (the name/company line) then the entire entry will be
deleted.

\(fn &optional RECORDS NOPROMPT)" t nil)

(autoload 'bbdb-delete-current-record "bbdb-com" "\
Delete the entire bbdb database entry which the cursor is within.
Pressing \\<bbdb-mode-map>\\[bbdb-apply-next-command-to-all-records] will
delete all records listed in the BBDB buffer.

\(fn RECS &optional NOPROMPT)" t nil)

(autoload 'bbdb-toggle-all-records-display-layout "bbdb-com" "\
Show all the fields of all visible records.
Like `bbdb-toggle-records-display-layout' but for all visible records.

\(fn ARG &optional RECORDS)" t nil)

(autoload 'bbdb-toggle-records-display-layout "bbdb-com" "\
Toggle whether the current record is displayed expanded or elided
\(multi-line or one-line display.)  With a numeric argument of 0, the
current record will unconditionally be made elided; with any other argument,
the current record will unconditionally be shown expanded.
\\<bbdb-mode-map>
If \"\\[bbdb-apply-next-command-to-all-records]\\[bbdb-toggle-records-display-layout]\" is used instead of simply \"\\[bbdb-toggle-records-display-layout]\", then the state of all records will
be changed instead of just the one at point.  In this case, an argument
of 0 means that all records will unconditionally be made elided; any other
numeric argument means that all of the records will unconditionally be shown
expanded; and no numeric argument means that the records are made to be in
the opposite state of the record under point.

\(fn ARG)" t nil)

(autoload 'bbdb-display-all-records-completely "bbdb-com" "\
Show all the fields of all currently displayed records.
The display layout `full-multi-line' is used for this.

\(fn ARG &optional RECORDS)" t nil)

(autoload 'bbdb-display-record-completely "bbdb-com" "\
Show all the fields of the current record.
The display layout `full-multi-line' is used for this.

\(fn ARG)" t nil)

(autoload 'bbdb-display-record-with-layout "bbdb-com" "\
Show all the fields of the current record using LAYOUT.

\(fn LAYOUT &optional RECORDS)" t nil)

(autoload 'bbdb-omit-record "bbdb-com" "\
Remove the current record from the display without deleting it from the
database.  With a prefix argument, omit the next N records.  If negative,
omit backwards.

\(fn N)" t nil)

(autoload 'bbdb-refile-record "bbdb-com" "\
Merge the current record into some other record; that is, delete the
record under point after copying all of the data within it into some other
record.  this is useful if you realize that somehow a redundant record has
gotten into the database, and you want to merge it with another.

If both records have names and/or companies, you are asked which to use.
Phone numbers, addresses, and network addresses are simply concatenated.
The first record is the record under the point; the second is prompted for.
Completion behaviour is as dictated by the variable `bbdb-completion-type'.

\(fn OLD-RECORD NEW-RECORD)" t nil)

(autoload 'bbdb-sort-notes "bbdb-com" "\
Sort the notes in the record according to `bbdb-notes-sort-order'.
Can be used in `bbdb-change-hook'.

\(fn REC)" nil nil)

(autoload 'bbdb-sort-phones "bbdb-com" "\
Sort the phones in the record according to the location.
Can be used in `bbdb-change-hook'.

\(fn REC)" nil nil)

(autoload 'bbdb-sort-addresses "bbdb-com" "\
Sort the addresses in the record according to the location.
Can be used in `bbdb-change-hook'.

\(fn REC)" nil nil)

(autoload 'bbdb-dwim-net-address "bbdb-com" "\
Return a string to use as the email address of the given record.
It is formatted like \"Firstname Lastname <addr>\" unless both the first name
and last name are constituents of the address, as in John.Doe@SomeHost, or the
address is already in the form \"Name <foo>\" or \"foo (Name)\", in which case
the address is used as-is.

If the record has the field 'mail-name it is used instead of the record's name.

If `bbdb-dwim-net-address-allow-redundancy' is non-nil, the name is always
included.  If `bbdb-dwim-net-address-allow-redundancy' is 'netonly the name is
never included!

A title is prepended from the field `bbdb-dwim-net-address-title-field' if it
exists.

\(fn RECORD &optional NET)" nil nil)

(autoload 'bbdb-send-mail "bbdb-com" "\
Compose a mail message to the person indicated by the current bbdb record.
The first (most-recently-added) address is used if there are more than one.
\\<bbdb-mode-map>
If \"\\[bbdb-apply-next-command-to-all-records]\\[bbdb-send-mail]\" is used instead of simply \"\\[bbdb-send-mail]\", then mail will be sent to all of the
folks listed in the *BBDB* buffer instead of just the person at point.

\(fn BBDB-RECORD &optional SUBJECT)" t nil)

(autoload 'bbdb-show-all-recipients "bbdb-com" "\
*Display BBDB records for all recipients of the message in this buffer.

\(fn)" t nil)

(autoload 'bbdb-completion-check-record "bbdb-com" "\
Not documented

\(fn SYM REC)" nil nil)

(autoload 'bbdb-completion-predicate "bbdb-com" "\
For use as the third argument to `completing-read'.
Obey the semantics of `bbdb-completion-type'.

\(fn SYMBOL)" nil nil)

(autoload 'bbdb-read-addresses-with-completion "bbdb-com" "\
Like `read-string', but allows `bbdb-complete-name' style completion.

\(fn PROMPT &optional DEFAULT)" nil nil)

(autoload 'bbdb-complete-name "bbdb-com" "\
Complete the user full-name or net-address before point (up to the
preceeding newline, colon, or comma, or the value of START-POS).  If
what has been typed is unique, insert an entry of the form \"User Name
<net-addr>\" (although see documentation for
bbdb-dwim-net-address-allow-redundancy).  If it is a valid completion
but not unique, a list of completions is displayed.

If the completion is done and `bbdb-complete-name-allow-cycling' is
true then cycle through the nets for the matching record.

When called with a prefix arg then display a list of all nets.

Completion behaviour can be controlled with `bbdb-completion-type'.

\(fn &optional START-POS)" t nil)

(autoload 'bbdb-yank "bbdb-com" "\
Insert the current contents of the *BBDB* buffer at point.

\(fn)" t nil)

(autoload 'bbdb-define-all-aliases "bbdb-com" "\
Define mail aliases for some of the records in the database.
Every record which has a `mail-alias' field (but see
`bbdb-define-all-aliases-field') will have a mail alias defined for it
which is the contents of that field.  If there are multiple
comma-separated words in this field, then all of those words will be
defined as aliases for that record.

If multiple entries in the database have the same mail alias, then
that alias expands to a comma-separated list of the primary network
addresses of all of those people.

An alias ending in \"*\" will expand to all the nets of the record.
An alias ending in \"[NTH]\" will expand the the NTH net of the
record.

Special nets exist and expand to other nets using one of
`bbdb-magic-net-*', `bbdb-magic-net-1' or `bbdb-magic-net-SOMETHING'.
Magic nets may not contain any comma character. If you need one, please
put it into a custom magic net function or use the octal escape
sequence \"\\054\".

Nets matching \"FUNCTION/ARG\" (i.e. containing at least one \"/\")
will be passed to the function `bbdb-magic-net-FUNCTION' with the
string argument ARG.

Nets starting with a \"(\" will be considered as a lisp list where the
first element is prefixed by `bbdb-magic-net-' and then called as a
function with the rest of the list as arguments.

Nets which do not contain an \"@\" character and also exist as aliases
are expanded recursively.  This can be used to define hierarchical
aliases.

Other nets are formatted by `bbdb-dwim-net-address'.

\(fn)" t nil)

(autoload 'bbdb-add-or-remove-mail-alias "bbdb-com" "\
Add NEWALIAS in all RECORDS or remove it if DELETE it t.
When called with prefix argument it will remove the alias.
We honor `bbdb-apply-next-command-to-all-records'!
The new alias will only be added if it isn't there yet.

\(fn &optional RECORDS NEWALIAS DELETE)" t nil)

(autoload 'bbdb-dial "bbdb-com" "\
Dial the number at point.
If the point is at the beginning of a record, dial the first
phone number.  Does not dial the extension.  Does not apply the
transformations from bbdb-dial-local-prefix-alist if a prefix arg
is given.

\(fn PHONE FORCE-AREA-CODE)" t nil)

(autoload 'bbdb-finger "bbdb-com" "\
Finger the network address of a BBDB record.
If this command is executed from the *BBDB* buffer, finger the network
address of the record at point; otherwise, it prompts for a user.
With a numeric prefix argument, finger the Nth network address of the
current record; with a prefix argument of ^U, finger all of them.
The *finger* buffer is filled asynchronously, meaning that you don't
have to wait around for it to finish; but fingering another user before
the first finger has finished could have unpredictable results.
\\<bbdb-mode-map>
If this command is executed from the *BBDB* buffer, it may be prefixed
with \"\\[bbdb-apply-next-command-to-all-records]\" (as in \"\\[bbdb-apply-next-command-to-all-records]\\[bbdb-finger]\" instead of simply \"\\[bbdb-finger]\"), meaning to finger all of
the users currently listed in the *BBDB* buffer instead of just the one
at point.  The numeric prefix argument has the same interpretation.

You can define a special network address to \"finger\" by defining a
field `finger-host' (default value of `bbdb-finger-host-field').

\(fn RECORD &optional WHICH-ADDRESS)" t nil)

(autoload 'bbdb-timestamp-older "bbdb-com" "\
*Display records with timestamp older than DATE.
DATE must be in yyyy-mm-dd format.

\(fn DATE)" t nil)

(autoload 'bbdb-timestamp-newer "bbdb-com" "\
*Display records with timestamp newer than DATE.
DATE must be in yyyy-mm-dd format.

\(fn DATE)" t nil)

(autoload 'bbdb-creation-older "bbdb-com" "\
*Display records with creation-date older than DATE.
DATE must be in yyyy-mm-dd format.

\(fn DATE)" t nil)

(autoload 'bbdb-creation-newer "bbdb-com" "\
*Display records with creation-date newer than DATE.
DATE must be in yyyy-mm-dd format.

\(fn DATE)" t nil)

(autoload 'bbdb-creation-no-change "bbdb-com" "\
*Display records that have the same timestamp and creation-date.

\(fn)" t nil)

(autoload 'bbdb-info "bbdb-com" "\
Not documented

\(fn)" t nil)

(autoload 'bbdb-help "bbdb-com" "\
Not documented

\(fn)" t nil)

(defvar bbdb-update-records-mode 'annotating "\
Controls how `bbdb-update-records' processes email addresses.
Set this to an expression which evaluates either to 'searching or
'annotating.  When set to 'annotating email addresses will be fed to
`bbdb-annotate-message-sender' in order to update existing records or create
new ones.  A value of 'searching will search just for existing records having
the right net.

There is a version of this variable for each MUA, which overrides this variable
when set!

This variable is also used for inter-function communication between the
functions `bbdb-update-records' and `bbdb-prompt-for-create'.")

(custom-autoload 'bbdb-update-records-mode "bbdb-com" t)

(autoload 'bbdb-update-records "bbdb-com" "\
Returns the records corresponding to the list of addresses ADDRS,
creating or modifying them as necessary.  A record will be created if
AUTO-CREATE-P is non-nil or if OFFER-TO-CREATE is true and the user
confirms the creation.

`bbdb-update-records-mode' controls if records are updated or not.
A MUA specific variable, e.g. `bbdb/vm-update-records-mode', can
overwrite this.

See also `bbdb-get-only-first-address-p' to limit the update to the
sender of the message.

When hitting C-g once you will not be asked any more for new people listed
in this message, but it will search only for existing records.  When hitting
C-g again it will stop scanning.

\(fn ADDRS AUTO-CREATE-P OFFER-TO-CREATE)" nil nil)

(defvar bbdb-get-addresses-headers '((authors "From" "Resent-From" "Reply-To") (recipients "Resent-To" "Resent-CC" "To" "CC" "BCC")) "\
*List of headers to search for senders and recipients email addresses.
The headers are grouped into two classes, the authors and the senders headers.")

(custom-autoload 'bbdb-get-addresses-headers "bbdb-com" t)

(defvar bbdb-get-only-first-address-p nil "\
*If t `bbdb-update-records' will return only the first one.
Changing this variable will show its effect only after clearing the
`bbdb-message-cache' of a folder or closing and visiting it again.")

(custom-autoload 'bbdb-get-only-first-address-p "bbdb-com" t)

;;;***

;;;### (autoloads (bbdb-field-edit-del bbdb-field-edit-add) "bbdb-edit"
;;;;;;  "bbdb/bits/bbdb-edit.el" (14977 21490))
;;; Generated autoloads from bbdb/bits/bbdb-edit.el

(autoload 'bbdb-field-edit-add "bbdb-edit" "\
Add VALUE to FIELD of bbdb-record(s).

\(fn BBDB-RECORD FIELD VALUE)" t nil)

(autoload 'bbdb-field-edit-del "bbdb-edit" "\
Delete VALUE to FIELD of bbdb-record(s).
If prefix arg exists, delete all existing field values matching VALUE(regexp).

\(fn BBDB-RECORD FIELD VALUE)" t nil)

;;;***

;;;### (autoloads (bbdb-create-ftp-site bbdb-ftp) "bbdb-ftp" "bbdb/lisp/bbdb-ftp.el"
;;;;;;  (18011 20884))
;;; Generated autoloads from bbdb/lisp/bbdb-ftp.el

(autoload 'bbdb-ftp "bbdb-ftp" "\
Use ange-ftp to open an ftp-connection to a BBDB record's name.
If this command is executed from the *BBDB* buffer, ftp the site of
the record at point; otherwise, it prompts for an ftp-site.

\(fn BBDB-RECORD &optional WHICH)" t nil)

(autoload 'bbdb-create-ftp-site "bbdb-ftp" "\
Add a new ftp-site entry to the bbdb database.
Prompts for all relevant info using the echo area,
inserts the new record in the db, sorted alphabetically.

\(fn RECORD)" t nil)

;;;***

;;;### (autoloads (bbdb-insinuate-gnus bbdb/gnus-summary-show-all-recipients
;;;;;;  bbdb/gnus-score bbdb/gnus-snarf-signature bbdb/gnus-show-all-recipients
;;;;;;  bbdb/gnus-show-records bbdb/gnus-annotate-sender bbdb/gnus-update-records
;;;;;;  bbdb/gnus-update-record) "bbdb-gnus" "bbdb/lisp/bbdb-gnus.el"
;;;;;;  (19383 21482))
;;; Generated autoloads from bbdb/lisp/bbdb-gnus.el

(autoload 'bbdb/gnus-update-record "bbdb-gnus" "\
Return the record corresponding to the current Gnus message, creating
or modifying it as necessary.  A record will be created if
bbdb/news-auto-create-p is non-nil, or if OFFER-TO-CREATE is true and
the user confirms the creation.

\(fn &optional OFFER-TO-CREATE)" nil nil)

(autoload 'bbdb/gnus-update-records "bbdb-gnus" "\
Return the records corresponding to the current Gnus message, creating
or modifying it as necessary.  A record will be created if
`bbdb/news-auto-create-p' is non-nil or if OFFER-TO-CREATE is true
and the user confirms the creation.

The variable `bbdb/gnus-update-records-mode' controls what actions
are performed and it might override `bbdb-update-records-mode'.

When hitting C-g once you will not be asked anymore for new people listed
in this message, but it will search only for existing records.  When hitting
C-g again it will stop scanning.

\(fn &optional OFFER-TO-CREATE)" nil nil)

(autoload 'bbdb/gnus-annotate-sender "bbdb-gnus" "\
Add a line to the end of the Notes field of the BBDB record
corresponding to the sender of this message.  If REPLACE is non-nil,
replace the existing notes entry (if any).

\(fn STRING &optional REPLACE)" t nil)

(autoload 'bbdb/gnus-show-records "bbdb-gnus" "\
Display the contents of the BBDB for all addresses of this message.
This buffer will be in `bbdb-mode', with associated keybindings.

\(fn &optional ADDRESS-CLASS)" t nil)

(autoload 'bbdb/gnus-show-all-recipients "bbdb-gnus" "\
Show all recipients of this message. Counterpart to `bbdb/vm-show-sender'.

\(fn)" t nil)

(autoload 'bbdb/gnus-snarf-signature "bbdb-gnus" "\
Snarf signature from the corresponding *Article* buffer.

\(fn)" t nil)

(autoload 'bbdb/gnus-score "bbdb-gnus" "\
This returns a score alist for Gnus.  A score pair will be made for
every member of the net field in records which also have a gnus-score
field.  This allows the BBDB to serve as a supplemental global score
file, with the advantage that it can keep up with multiple and changing
addresses better than the traditionally static global scorefile.

\(fn GROUP)" nil nil)

(autoload 'bbdb/gnus-summary-show-all-recipients "bbdb-gnus" "\
Display BBDB records for all recipients of the message.

\(fn NOT-ELIDED)" t nil)

(autoload 'bbdb-insinuate-gnus "bbdb-gnus" "\
Call this function to hook BBDB into Gnus.

\(fn)" nil nil)

;;;***

;;;### (autoloads (bbdb-menu bbdb-fontify-buffer) "bbdb-gui" "bbdb/lisp/bbdb-gui.el"
;;;;;;  (19383 21413))
;;; Generated autoloads from bbdb/lisp/bbdb-gui.el

(autoload 'bbdb-fontify-buffer "bbdb-gui" "\
Not documented

\(fn &optional RECORDS)" t nil)

(autoload 'bbdb-menu "bbdb-gui" "\
Not documented

\(fn EVENT)" t nil)

;;;***

;;;### (autoloads (bbdb-force-record-create sample-bbdb-canonicalize-net-hook
;;;;;;  bbdb-auto-notes-hook bbdb-ignore-some-messages-hook bbdb-ignore-selected-messages-hook
;;;;;;  bbdb-ignore-most-messages-hook bbdb-extract-field-value bbdb-header-start
;;;;;;  bbdb-creation-date-hook bbdb-timestamp-hook) "bbdb-hooks"
;;;;;;  "bbdb/lisp/bbdb-hooks.el" (19383 20853))
;;; Generated autoloads from bbdb/lisp/bbdb-hooks.el

(autoload 'bbdb-timestamp-hook "bbdb-hooks" "\
For use as a `bbdb-change-hook'; maintains a notes-field called `timestamp'
for the given record which contains the time when it was last modified.  If
there is such a field there already, it is changed, otherwise it is added.

\(fn RECORD)" nil nil)

(autoload 'bbdb-creation-date-hook "bbdb-hooks" "\
For use as a `bbdb-create-hook'; adds a notes-field called `creation-date'
which is the current time string.

\(fn RECORD)" nil nil)

(autoload 'bbdb-header-start "bbdb-hooks" "\
Returns a marker at the beginning of the header block of the current
message.  This will not necessarily be in the current buffer.

\(fn)" nil nil)

(autoload 'bbdb-extract-field-value "bbdb-hooks" "\
Given the name of a field (like \"Subject\") this returns the value of
that field in the current message, or nil.  This works whether you're in
Gnus, Rmail, or VM.  This works on multi-line fields, but if more than
one field of the same name is present, only the last is returned.  It is
expected that the current buffer has a message in it, and (point) is at the
beginning of the message headers.

\(fn FIELD-NAME)" nil nil)

(autoload 'bbdb-ignore-most-messages-hook "bbdb-hooks" "\
For use as the value of bbdb/news-auto-create-p or bbdb/mail-auto-create-p.
This will automatically create BBDB entries for messages which match
the bbdb-ignore-most-messages-alist (which see) and *no* others.

\(fn &optional INVERT-SENSE)" nil nil)

(autoload 'bbdb-ignore-selected-messages-hook "bbdb-hooks" "\
For use as a bbdb/news-auto-create-hook or bbdb/mail-auto-create-hook.
This will automatically create BBDB entries for messages based on a
combination of bbdb-ignore-some-messages-alist and
bbdb-ignore-most-messages-alist.  It first looks at the SOME list.  If
that doesn't disqualify a message, then it looks at the MOST list.  If
that qualifies the message, the record is auto-created, but a
confirmation is conditionally sought, based on the value of
`bbdb-ignore-selected-messages-confirmation'.

\(fn)" nil nil)

(autoload 'bbdb-ignore-some-messages-hook "bbdb-hooks" "\
For use as a `bbdb/news-auto-create-hook' or `bbdb/mail-auto-create-hook'.
This will automatically create BBDB entries for messages which do *not*
match the `bbdb-ignore-some-messages-alist' (which see).

\(fn)" nil nil)

(autoload 'bbdb-auto-notes-hook "bbdb-hooks" "\
For use as a `bbdb-notice-hook'.  This might automatically add some text
to the notes field of the BBDB record corresponding to the current record
based on the header of the current message.  See the documentation for
the variables `bbdb-auto-notes-alist' and `bbdb-auto-notes-ignore'.

\(fn RECORD)" nil nil)

(autoload 'sample-bbdb-canonicalize-net-hook "bbdb-hooks" "\
Not documented

\(fn ADDR)" nil nil)

(autoload 'bbdb-force-record-create "bbdb-hooks" "\
Force automatic creation of a BBDB records for the current message.
You might add this to the reply hook of your MUA in order to automatically
get records added for those people you reply to.

\(fn)" t nil)

;;;***

;;;### (autoloads (bbdb-merge-file bbdb-merge-record) "bbdb-merge"
;;;;;;  "bbdb/lisp/bbdb-merge.el" (19383 21243))
;;; Generated autoloads from bbdb/lisp/bbdb-merge.el

(autoload 'bbdb-merge-record "bbdb-merge" "\
Generic merge function.

Merges new-record into your bbdb, using DATE to check who's more
up-to-date and OVERRIDE to decide who gets precedence if two dates
match. DATE can be extracted from a notes if it's an alist with an
element marked timestamp. Set OVERRIDE to 'new to allow the new record
to stomp on existing data, 'old to preserve existing data or nil to
merge both together. If it can't find a record to merge with, it will
create a new record. If MERGE-RECORD is set, it's a record discovered
by other means that should be merged with.

Returns the Grand Unified Record.

\(fn NEW-RECORD &optional MERGE-RECORD OVERRIDE)" nil nil)

(autoload 'bbdb-merge-file "bbdb-merge" "\
Merge a bbdb file into the in-core bbdb.

\(fn &optional BBDB-NEW OVERRIDE MATCH-FUN)" t nil)

;;;***

;;;### (autoloads (bbdb-insinuate-mh bbdb/mh-show-sender bbdb/mh-annotate-sender
;;;;;;  bbdb/mh-update-record) "bbdb-mhe" "bbdb/lisp/bbdb-mhe.el"
;;;;;;  (17967 44425))
;;; Generated autoloads from bbdb/lisp/bbdb-mhe.el

(autoload 'bbdb/mh-update-record "bbdb-mhe" "\
Returns the record corresponding to the current MH message, creating or
modifying it as necessary.  A record will be created if
bbdb/mail-auto-create-p is non-nil, or if OFFER-TO-CREATE is true and
the user confirms the creation.

\(fn &optional OFFER-TO-CREATE)" nil nil)

(autoload 'bbdb/mh-annotate-sender "bbdb-mhe" "\
Add a line to the end of the Notes field of the BBDB record
corresponding to the sender of this message.  If REPLACE is non-nil,
replace the existing notes entry (if any).

\(fn STRING &optional REPLACE)" t nil)

(autoload 'bbdb/mh-show-sender "bbdb-mhe" "\
Display the contents of the BBDB for the sender of this message.
This buffer will be in bbdb-mode, with associated keybindings.

\(fn)" t nil)

(autoload 'bbdb-insinuate-mh "bbdb-mhe" "\
Call this function to hook BBDB into MH-E.

\(fn)" nil nil)

;;;***

;;;### (autoloads (bbdb-migrate-update-file-version bbdb-migrate-rewrite-all
;;;;;;  bbdb-unmigrate-record bbdb-migrate bbdb-migration-query)
;;;;;;  "bbdb-migrate" "bbdb/lisp/bbdb-migrate.el" (18011 20885))
;;; Generated autoloads from bbdb/lisp/bbdb-migrate.el

(autoload 'bbdb-migration-query "bbdb-migrate" "\
Ask if the database is to be migrated.
ONDISK is the version number of the database as currently stored on
disk.  Returns the version for the saved database.

\(fn ONDISK)" nil nil)

(autoload 'bbdb-migrate "bbdb-migrate" "\
Migrate the BBDB from the version on disk (the car of
`bbdb-file-format-migration') to the current version (in
`bbdb-file-format').

\(fn RECORDS)" nil nil)

(autoload 'bbdb-unmigrate-record "bbdb-migrate" "\
Reverse-migrate a single record from the current version (in
`bbdb-file-format') to the version to be saved (the cdr of
`bbdb-file-format-migration').

\(fn RECORD)" nil nil)

(autoload 'bbdb-migrate-rewrite-all "bbdb-migrate" "\
Rewrite each and every record in the bbdb file; this is necessary if we
are updating an old file format.  MESSAGE-P says whether to sound off
for each record converted.  If RECORDS is non-nil, its value will be
used as the list of records to update.

\(fn MESSAGE-P &optional RECORDS)" nil nil)

(autoload 'bbdb-migrate-update-file-version "bbdb-migrate" "\
Change the `file-version' string from the OLD version to the NEW
version.

\(fn OLD NEW)" nil nil)

;;;***

;;;### (autoloads (bbdb-obsolete-net-canonicalize-net-hook) "bbdb-obsolete"
;;;;;;  "bbdb/bits/bbdb-obsolete.el" (15646 18903))
;;; Generated autoloads from bbdb/bits/bbdb-obsolete.el

(autoload 'bbdb-obsolete-net-canonicalize-net-hook "bbdb-obsolete" "\
Return user's current net address given obsolete ADDR.

Return ADDR if it is not obsolete anywhere, or there is no net address
in the matching record.  The field is set in `bbdb-obsolete-net-field'.

\(fn ADDR)" nil nil)

;;;***

;;;### (autoloads (bbdb-utilities-pgp) "bbdb-pgp" "bbdb/bits/bbdb-pgp.el"
;;;;;;  (16183 22987))
;;; Generated autoloads from bbdb/bits/bbdb-pgp.el

(let ((loads (get 'bbdb-utilities-pgp 'custom-loads))) (if (member '"bbdb-pgp" loads) nil (put 'bbdb-utilities-pgp 'custom-loads (cons '"bbdb-pgp" loads))))

;;;***

;;;### (autoloads (bbdb-print) "bbdb-print" "bbdb/lisp/bbdb-print.el"
;;;;;;  (19383 21601))
;;; Generated autoloads from bbdb/lisp/bbdb-print.el

(autoload 'bbdb-print "bbdb-print" "\
Make a TeX file for printing out the bbdb database.\\<bbdb-mode-map>
If \"\\[bbdb-apply-next-command-to-all-records]\\[bbdb-print]\" is used instead of simply \"\\[bbdb-print]\", then includes only the
people currently in the *BBDB* buffer.  With a prefix argument, makes
a brief (one-line-per-entry) printout.

There are various variables for customizing the content & format of
the printout, notably the variables `bbdb-print-alist' and
`bbdb-print-require'.  See the file bbdb-print.el for more information.

\(fn VISIBLE-RECORDS TO-FILE BRIEF)" t nil)

;;;***

;;;### (autoloads (bbdb-insinuate-reportmail) "bbdb-reportmail" "bbdb/lisp/bbdb-reportmail.el"
;;;;;;  (17884 52100))
;;; Generated autoloads from bbdb/lisp/bbdb-reportmail.el

(autoload 'bbdb-insinuate-reportmail "bbdb-reportmail" "\
Call this function to hook BBDB into reportmail.

\(fn)" nil nil)

;;;***

;;;### (autoloads (bbdb-snarf-region-better bbdb-print-latex bbdb-print-latex-list
;;;;;;  bbdb-csv-export bbdb-snarf-format bbdb/vm-grab-homepage bbdb-clean-nets
;;;;;;  rf-bbdb/vm-ignore-old-addresses rf-bbdb/vm-ignore-old-folders
;;;;;;  rf-bbdb/vm-primary-inbox-regexp bbdb-sms-yank-mobiles bbdb-send-sms
;;;;;;  bbdb/sms-max-chars bbdb/sms-mobile-field bbdb/sms-headers
;;;;;;  bbdb/sms-gateway) "bbdb-rf" "bbdb-rf.el" (19406 52035))
;;; Generated autoloads from bbdb-rf.el

(defvar bbdb/sms-gateway "sms.web.de" "\
A regexp matching folder names of primary VM inboxes.")

(custom-autoload 'bbdb/sms-gateway "bbdb-rf" t)

(defvar bbdb/sms-headers (if (fboundp 'bbdb-replace-in-string) (list (list "From" (concat (bbdb-replace-in-string (user-full-name) " " ".") "@web.de")) (list "Subject" (concat (bbdb-replace-in-string (user-full-name) " " ".") "@web.de")) '("Organization" nil))) "\
A list of (header value) pairs.
It replaces existing headers in the message buffer.
If value is nil the header will be removed.")

(custom-autoload 'bbdb/sms-headers "bbdb-rf" t)

(defvar bbdb/sms-mobile-field "mobile" "\
The label of the phones containing the mobile number.")

(custom-autoload 'bbdb/sms-mobile-field "bbdb-rf" t)

(defvar bbdb/sms-max-chars 160 "\
The maximum number of characters of a SMS.")

(custom-autoload 'bbdb/sms-max-chars "bbdb-rf" t)

(autoload 'bbdb-send-sms "bbdb-rf" "\
Compose a SMS message to the persons in RECORDS with SUBJECT.

\(fn RECORDS &optional SUBJECT)" t nil)

(autoload 'bbdb-sms-yank-mobiles "bbdb-rf" "\
Add people displayed in the *BBDB* buffer on this SMS recipients list.

\(fn)" t nil)

(defvar rf-bbdb/vm-primary-inbox-regexp (if (boundp 'vm-primary-inbox) vm-primary-inbox) "\
A regexp matching folder names of primary VM inboxes.")

(custom-autoload 'rf-bbdb/vm-primary-inbox-regexp "bbdb-rf" t)

(autoload 'rf-bbdb/vm-ignore-old-folders "bbdb-rf" "\
Hook for ignoring all folders except in-boxes.

Set `bbdb/mail-auto-create-p' to this function in order to ignore new
addresses in all folders except the `vm-primary-inbox' or those matching
`bbdb/vm-primary-inbox-regexp'.

\(fn)" t nil)

(autoload 'rf-bbdb/vm-ignore-old-addresses "bbdb-rf" "\
Hook for ignoring all addresses except in in-boxes.

Set `bbdb-always-add-addresses' to this function in order to ignore new
addresses in all folders except the `vm-primary-inbox' or those matching
`bbdb/vm-primary-inbox-regexp'.

\(fn)" nil nil)

(autoload 'bbdb-clean-nets "bbdb-rf" "\
Remove dublicate nets from all visible RECORDS.

\(fn RECORDS)" t nil)

(autoload 'bbdb/vm-grab-homepage "bbdb-rf" "\
Grab the current URL and store it in the record correspondig to the sender.

\(fn)" t nil)

(defvar bbdb-snarf-format '(("default" (name 1 "^.*$") (phones 3 "^\\(\\w+:\\)?\\s *[+()0-9][0-9() 	/-]+[0-9]+$") (nets 3 "[^ 	\n<]+@[^ 	\n>]+") (www 1 "\\(http://\\|www.\\)[^ 	\n]+") (street 1 "^\\w+.*[0-9/-]+\\w*.*") (zip 1 "\\([A-Z]-\\)?[0-9][0-9][0-9][0-9]+") (city 1 "\\w+[^ ,\n]*") (state 1 "\\w+") (country 1 "\\w+"))) "\
*An alist of snarfing strategies for `bbdb-snarf-region-better'.

The keys of the alist are the strategy names.

A strategy is a list of the following elements:

    (BBDB-FIELD-NAME COUNT REGEXP)

During snarfing the elements of the strategy will be processed in sequential
order.  REGEXP will be searched for COUNT times and if a match occurs it will
be appended to the bbdb field BBDB-FIELD-NAME and is removed from the snarfing
buffer.  The unmatched rest goes to the notes.")

(custom-autoload 'bbdb-snarf-format "bbdb-rf" t)

(autoload 'bbdb-csv-export "bbdb-rf" "\
Export all currently displayed records to FILE honoring EXPORT-TYPE.
The records are written in CVS-format (comma separated values) which can be
imported by nearly all clients, i.e. Outlook, Netscape, Mozilla, KAddress,
... each client expects a specific layout which can be set by export-type.

See `bbdb-csv-export-fields` for predefined export-types.

\(fn &optional FILE EXPORT-TYPE)" t nil)

(autoload 'bbdb-print-latex-list "bbdb-rf" "\
List all BBDB records matching PREDICATE.

Predicate is a regexp matching the value of `bbdb-print-latex-list-field`.

\(fn PREDICATE)" t nil)

(autoload 'bbdb-print-latex "bbdb-rf" "\
Writes currently visible records in LaTeX format TO-FILE.

Make a LaTeX file for printing out the bbdb database.\\<bbdb-mode-map>
If \"\\[bbdb-apply-next-command-to-all-records]\\[bbdb-print]\" is used instead of simply \"\\[bbdb-print]\", then includes only the
people currently in the *BBDB* buffer.

There are various variables for customizing the content & format of
the printout, notably the variables `bbdb-print-alist' and
`bbdb-print-require'.  See the file bbdb-print.el for more information.

And probably you also want the TeX source for this!
They are on the same site as this file!

\(fn TO-FILE)" t nil)

(autoload 'bbdb-snarf-region-better "bbdb-rf" "\
Snarf up a BBDB record in the region from BEGIN to END.
I tried a better snarfing here.  See `bbdb-snarf' for the original.

When called with a prefix arg, prompt for a strategy.

\(fn BEGIN END)" t nil)

;;;***

;;;### (autoloads (bbdb-insinuate-rmail bbdb/rmail-show-sender bbdb/rmail-annotate-sender
;;;;;;  bbdb/rmail-update-records bbdb/rmail-update-record) "bbdb-rmail"
;;;;;;  "bbdb/lisp/bbdb-rmail.el" (17961 17377))
;;; Generated autoloads from bbdb/lisp/bbdb-rmail.el

(autoload 'bbdb/rmail-update-record "bbdb-rmail" "\
Not documented

\(fn &optional OFFER-TO-CREATE)" nil nil)

(autoload 'bbdb/rmail-update-records "bbdb-rmail" "\
Returns the records corresponding to the current RMAIL emssage,
creating or modifying them as necessary.  A record will be created if
bbdb/mail-auto-create-p is non-nil or if OFFER-TO-CREATE is true, and
the user confirms the creation.

The variable `bbdb/rmail-update-records-mode' controls what actions
are performed and it might override `bbdb-update-records-mode'.

When hitting C-g once you will not be asked anymore for new people
listed n this message, but it will search only for existing records.
When hitting C-g again it will stop scanning.

\(fn &optional OFFER-TO-CREATE)" nil nil)

(autoload 'bbdb/rmail-annotate-sender "bbdb-rmail" "\
Add a line to the end of the Notes field of the BBDB record
corresponding to the sender of this message.  If REPLACE is non-nil,
replace the existing notes entry (if any).

\(fn STRING &optional REPLACE)" t nil)

(autoload 'bbdb/rmail-show-sender "bbdb-rmail" "\
Display the contents of the BBDB for the sender of this message.
This buffer will be in bbdb-mode, with associated keybindings.

\(fn)" t nil)

(autoload 'bbdb-insinuate-rmail "bbdb-rmail" "\
Call this function to hook BBDB into RMAIL.

\(fn)" nil nil)

;;;***

;;;### (autoloads (bbdb-insinuate-sc bbdb/sc-default) "bbdb-sc" "bbdb/lisp/bbdb-sc.el"
;;;;;;  (17884 52100))
;;; Generated autoloads from bbdb/lisp/bbdb-sc.el

(autoload 'bbdb/sc-default "bbdb-sc" "\
If the current \"from\" field in `sc-mail-info' alist
contains only an e-mail address, lookup e-mail address in
BBDB, and prepend a new \"from\" field to `sc-mail-info'.

\(fn)" nil nil)

(autoload 'bbdb-insinuate-sc "bbdb-sc" "\
Call this function to hook BBDB into Supercite.

\(fn)" nil nil)

;;;***

;;;### (autoloads (bbdb-rfc822-addresses bbdb-extract-address-components
;;;;;;  bbdb-snarf-region bbdb-snarf) "bbdb-snarf" "bbdb/lisp/bbdb-snarf.el"
;;;;;;  (17961 17312))
;;; Generated autoloads from bbdb/lisp/bbdb-snarf.el

(autoload 'bbdb-snarf "bbdb-snarf" "\
snarf up a bbdb record WHERE the point is.
We assume things are line-broken and paragraph-bounded.
The name comes first and other fields (address,
phone, email, web pages) are recognized by context.

Required context:
    addresses end with \"City, State ZIP\" or \"City, State\"
    phones match bbdb-snarf-phone-regexp
        (currently US-style phones)
    e-mail addresses have @'s in them
    web sites are recognized by http:// or www.

Address and phone context are currently US-specific;
patches to internationalize these assumptions are welcome.

\\[bbdb-snarf] is similar to \\[bbdb-whois-sentinel], but less specialized.

\(fn WHERE)" t nil)

(autoload 'bbdb-snarf-region "bbdb-snarf" "\
snarf up a bbdb record in the current region.  See `bbdb-snarf' for
more details.

\(fn BEGIN END)" t nil)

(autoload 'bbdb-extract-address-components "bbdb-snarf" "\
Return a list of address components found in ADSTRING.
If extracting fails one probably has to adjust the variable
`bbdb-extract-address-component-regexps'.

\(fn ADSTRING &optional IGNORE-ERRORS)" nil nil)

(autoload 'bbdb-rfc822-addresses "bbdb-snarf" "\
Split ADDRLINE into a list of parsed addresses.

You can't do this with rfc822.el in any sort of useful way because it discards
the comments. You can't do this with mail-extr.el because the multiple address
parsing in GNU Emacs appears to be broken beyond belief, and the XEmacs
version doesn't support multiple addresses.

\(fn ADDRLINE &optional IGNORE-ERRORS)" nil nil)

;;;***

;;;### (autoloads (bbdb-srv-add-phone bbdb/srv-auto-create-mail-news-dispatcher
;;;;;;  bbdb/srv-handle-headers-with-delay) "bbdb-srv" "bbdb/lisp/bbdb-srv.el"
;;;;;;  (17884 52100))
;;; Generated autoloads from bbdb/lisp/bbdb-srv.el

(autoload 'bbdb/srv-handle-headers-with-delay "bbdb-srv" "\
Just like bbdb/srv-handle-headers, but only updates every few seconds.
This is so that trying to display many records in succession won't queue them
up, but will end up only displaying a record when no displays have been
requested for a couple of seconds.

\(fn HEADERS)" nil nil)

(defalias 'bbdb-srv 'bbdb/srv-handle-headers-with-delay)

(autoload 'bbdb/srv-auto-create-mail-news-dispatcher "bbdb-srv" "\
For use as the value of bbdb/srv-auto-create-p.
This will try to decide if this is a mail message or a news message, and then
run either bbdb/news-auto-create-p or bbdb/mail-auto-create-p as appropriate.
\(The heuristic is that news messages never have a Status or X-Mozilla-Status
header; and that mail messages never have Path headers.)

\(fn)" nil nil)

(autoload 'bbdb-srv-add-phone "bbdb-srv" "\
Not documented

\(fn PHONE-STRING &optional DESCRIPTION RECORD)" nil nil)

;;;***

;;;### (autoloads (bbdb-insinuate-vm bbdb/vm-set-auto-folder-alist
;;;;;;  bbdb/vm-set-auto-folder-alist-headers bbdb/vm-set-auto-folder-alist-field
;;;;;;  bbdb/vm-show-sender bbdb/vm-show-all-recipients bbdb/vm-show-records
;;;;;;  bbdb/vm-annotate-sender bbdb/vm-update-records bbdb/vm-update-record)
;;;;;;  "bbdb-vm" "bbdb/lisp/bbdb-vm.el" (18389 44231))
;;; Generated autoloads from bbdb/lisp/bbdb-vm.el

(autoload 'bbdb/vm-update-record "bbdb-vm" "\
Not documented

\(fn &optional OFFER-TO-CREATE)" nil nil)

(autoload 'bbdb/vm-update-records "bbdb-vm" "\
Returns the records corresponding to the current VM message,
creating or modifying them as necessary.  A record will be created if
`bbdb/mail-auto-create-p' is non-nil or if OFFER-TO-CREATE is true, and
the user confirms the creation.

The variable `bbdb/vm-update-records-mode' controls what actions
are performed and it might override `bbdb-update-records-mode'.

When hitting C-g once you will not be asked anymore for new people listed
in this message, but it will search only for existing records.  When hitting
C-g again it will stop scanning.

\(fn &optional OFFER-TO-CREATE)" nil nil)

(autoload 'bbdb/vm-annotate-sender "bbdb-vm" "\
Add a line to the end of the Notes field of the BBDB record
corresponding to the sender of this message.  If REPLACE is non-nil,
replace the existing notes entry (if any).

\(fn STRING &optional REPLACE)" t nil)

(autoload 'bbdb/vm-show-records "bbdb-vm" "\
Display the contents of the BBDB for the sender of this message.
This buffer will be in bbdb-mode, with associated keybindings.

\(fn &optional ADDRESS-CLASS)" t nil)

(autoload 'bbdb/vm-show-all-recipients "bbdb-vm" "\
Show all recipients of this message. Counterpart to `bbdb/vm-show-sender'.

\(fn)" t nil)

(autoload 'bbdb/vm-show-sender "bbdb-vm" "\
Display the contents of the BBDB for the senders of this message.
With a prefix argument show the recipients instead,
with two prefix arguments show all records.
This buffer will be in `bbdb-mode', with associated keybindings.

\(fn &optional SHOW-RECIPIENTS)" t nil)

(defvar bbdb/vm-set-auto-folder-alist-field 'vm-folder "\
*The field which `bbdb/vm-set-auto-folder-alist' searches for.")

(custom-autoload 'bbdb/vm-set-auto-folder-alist-field "bbdb-vm" t)

(defvar bbdb/vm-set-auto-folder-alist-headers '("From:" "To:" "CC:") "\
*The headers used by `bbdb/vm-set-auto-folder-alist'.
The order in this list is the order how matching will be performed!")

(custom-autoload 'bbdb/vm-set-auto-folder-alist-headers "bbdb-vm" t)

(autoload 'bbdb/vm-set-auto-folder-alist "bbdb-vm" "\
Create a `vm-auto-folder-alist' according to the records in the bbdb.
For each record that has a 'vm-folder' attribute, add an
element (email-regexp . folder) to the `vm-auto-folder-alist'.

The element gets added to the 'element-name' sublist of the
`vm-auto-folder-alist'.

The car of the element consists of all the email addresses for the
bbdb record concatenated with OR; the cdr is the value of the
vm-folder attribute.

If the first character of vm-folders value is a quote ' it will be
parsed as lisp expression and is evaluated to return a folder name,
e.g. define you own function `my-folder-name' and set it to
        '(my-folder-name)

\(fn)" t nil)

(autoload 'bbdb-insinuate-vm "bbdb-vm" "\
Call this function to hook BBDB into VM.

\(fn)" nil nil)

;;;***

;;;### (autoloads (bbdb-insinuate-w3 bbdb-www-grab-homepage bbdb-www)
;;;;;;  "bbdb-w3" "bbdb/lisp/bbdb-w3.el" (17884 52100))
;;; Generated autoloads from bbdb/lisp/bbdb-w3.el

(autoload 'bbdb-www "bbdb-w3" "\
Visit URLs stored in the `www' field of the current record.
\\[bbdb-apply-next-command-to-all-records]\\[bbdb-www] means to try all records currently visible.
Non-interactively, do all records if arg is nonnil.

\(fn REC &optional WHICH)" t nil)

(autoload 'bbdb-www-grab-homepage "bbdb-w3" "\
Grab the current URL and store it in the bbdb database

\(fn RECORD)" t nil)

(autoload 'bbdb-insinuate-w3 "bbdb-w3" "\
Call this function to hook BBDB into W3.

\(fn)" nil nil)

;;;***

;;;### (autoloads (bbdb-whois) "bbdb-whois" "bbdb/lisp/bbdb-whois.el"
;;;;;;  (19383 21198))
;;; Generated autoloads from bbdb/lisp/bbdb-whois.el

(autoload 'bbdb-whois "bbdb-whois" "\
Not documented

\(fn THE-RECORD &optional SERVER)" t nil)

;;;***

;;;### (autoloads (bbdb-load-touchtones bbdb-sound-volume bbdb-sounds-directory
;;;;;;  bbdb-xemacs-display-completion-list) "bbdb-xemacs" "bbdb/lisp/bbdb-xemacs.el"
;;;;;;  (18011 20885))
;;; Generated autoloads from bbdb/lisp/bbdb-xemacs.el

(autoload 'bbdb-xemacs-display-completion-list "bbdb-xemacs" "\
Wrapper for `display-completion-list'.
Allows callbacks on XEmacs `display-completion-list' is called with
`:activate-callback CALLBACK' if CALLBACK is non-nil.
`:user-data DATA' is also used if DATA is non-nil.
Neither are used if CALLBACK is nil.

\(fn LIST &optional CALLBACK DATA)" nil nil)

(defvar bbdb-sounds-directory (expand-file-name "~/.xemacs/etc/sounds") "\
The directory to load the touchtone sound files from, or nil if none.")

(custom-autoload 'bbdb-sounds-directory "bbdb-xemacs" t)

(defvar bbdb-sound-volume 50 "\
Volume for playing sounds.")

(custom-autoload 'bbdb-sound-volume "bbdb-xemacs" t)

(autoload 'bbdb-load-touchtones "bbdb-xemacs" "\
Load the touchtone sounds into `sound-alist'.
The directory specified in `bbdb-sounds-directory' is searched for the files
touchtone.*\\.\\(wav\\|au\\) as named in `bbdb-sound-files'.
They are stored in `sound-alist' as touchtone0 to touchtone11.

\(fn)" t nil)

;;;***

;;;### (autoloads (bison->wisent) "bison-wisent" "cedet/semantic/wisent/bison-wisent.el"
;;;;;;  (17213 40596))
;;; Generated autoloads from cedet/semantic/wisent/bison-wisent.el

(autoload 'bison->wisent "bison-wisent" "\
Treat the current buffer as a YACC or BISON file, and translate to wisent.
Replaces all comments with wisent compatible comments.
Finds % commands that wisent cannot handle, and comments them out.
Deletes all actions, replacing them with small comments.

\(fn)" t nil)

;;;***

;;;### (autoloads (bmkp-version) "bookmark+" "bookmark-plus/bookmark+.el"
;;;;;;  (19804 63180))
;;; Generated autoloads from bookmark-plus/bookmark+.el

(defconst bmkp-version-number "3.2.0")

(autoload 'bmkp-version "bookmark+" "\
Show version number of library `bookmark+.el'.

\(fn)" t nil)

;;;***

;;;### (autoloads (bmkp-delete-bookmarks bmkp-delete-all-autonamed-for-this-buffer
;;;;;;  bmkp-set-autonamed-regexp-region bmkp-set-autonamed-regexp-buffer
;;;;;;  bmkp-set-autonamed-bookmark-at-line bmkp-set-autonamed-bookmark
;;;;;;  bmkp-toggle-autonamed-bookmark-set/delete bmkp-previous-bookmark-w32-repeat
;;;;;;  bmkp-next-bookmark-w32-repeat bmkp-previous-bookmark-w32
;;;;;;  bmkp-next-bookmark-w32 bmkp-previous-bookmark-this-buffer-repeat
;;;;;;  bmkp-next-bookmark-this-buffer-repeat bmkp-previous-bookmark-this-buffer
;;;;;;  bmkp-next-bookmark-this-buffer bmkp-previous-bookmark-repeat
;;;;;;  bmkp-next-bookmark-repeat bmkp-previous-bookmark bmkp-next-bookmark
;;;;;;  bmkp-cycle-this-buffer-other-window bmkp-cycle-this-buffer
;;;;;;  bmkp-cycle-other-window bmkp-cycle bmkp-jump-in-navlist-other-window
;;;;;;  bmkp-jump-in-navlist bmkp-some-tags-regexp-jump-other-window
;;;;;;  bmkp-some-tags-regexp-jump bmkp-some-tags-jump-other-window
;;;;;;  bmkp-some-tags-jump bmkp-all-tags-regexp-jump-other-window
;;;;;;  bmkp-all-tags-regexp-jump bmkp-all-tags-jump-other-window
;;;;;;  bmkp-all-tags-jump bmkp-w3m-jump-other-window bmkp-w3m-jump
;;;;;;  bmkp-url-jump-other-window bmkp-url-jump bmkp-variable-list-jump
;;;;;;  bmkp-this-buffer-jump-other-window bmkp-this-buffer-jump
;;;;;;  bmkp-specific-files-jump-other-window bmkp-specific-files-jump
;;;;;;  bmkp-specific-buffers-jump-other-window bmkp-specific-buffers-jump
;;;;;;  bmkp-remote-file-jump-other-window bmkp-remote-file-jump
;;;;;;  bmkp-region-jump-other-window bmkp-region-jump bmkp-non-file-jump-other-window
;;;;;;  bmkp-non-file-jump bmkp-man-jump-other-window bmkp-man-jump
;;;;;;  bmkp-local-file-jump-other-window bmkp-local-file-jump bmkp-info-jump-other-window
;;;;;;  bmkp-info-jump bmkp-gnus-jump-other-window bmkp-gnus-jump
;;;;;;  bmkp-file-jump-other-window bmkp-file-jump bmkp-dired-jump-current-other-window
;;;;;;  bmkp-dired-jump-current bmkp-dired-jump-other-window bmkp-dired-jump
;;;;;;  bmkp-desktop-jump bmkp-bookmark-list-jump bmkp-autonamed-this-buffer-jump-other-window
;;;;;;  bmkp-autonamed-this-buffer-jump bmkp-autonamed-jump-other-window
;;;;;;  bmkp-autonamed-jump bmkp-jump-to-type-other-window bmkp-jump-to-type
;;;;;;  bmkp-dired-subdirs bmkp-set-variable-list-bookmark bmkp-desktop-delete
;;;;;;  bmkp-desktop-read bmkp-desktop-change-dir bmkp-set-desktop-bookmark
;;;;;;  bmkp-bookmark-file-jump bmkp-set-bookmark-file-bookmark bmkp-list-defuns-in-commands-file
;;;;;;  bmkp-describe-bookmark-internals bmkp-describe-bookmark bmkp-file-target-set
;;;;;;  bmkp-url-target-set bmkp-rename-tag bmkp-remove-tags-from-all
;;;;;;  bmkp-remove-tags bmkp-set-tag-value bmkp-set-tag-value-for-navlist
;;;;;;  bmkp-add-tags bmkp-remove-all-tags bmkp-list-all-tags bmkp-unomit-all
;;;;;;  bmkp-navlist-bmenu-list bmkp-this-buffer-bmenu-list bmkp-choose-navlist-of-type
;;;;;;  bmkp-choose-navlist-from-bookmark-list bmkp-crosshairs-highlight
;;;;;;  bmkp-empty-file bmkp-use-bookmark-file-create bmkp-switch-to-last-bookmark-file
;;;;;;  bmkp-switch-bookmark-file bmkp-make-function-bookmark bmkp-toggle-saving-bookmark-file
;;;;;;  bmkp-toggle-saving-menu-list-state bmkp-toggle-bookmark-set-refreshes
;;;;;;  bmkp-send-bug-report bmkp-edit-bookmark-record-send bmkp-edit-bookmark-record
;;;;;;  bmkp-edit-bookmark bookmark-load bookmark-save bookmark-delete
;;;;;;  bookmark-insert bookmark-rename bookmark-insert-location
;;;;;;  bookmark-relocate bookmark-jump-other-window bookmark-jump
;;;;;;  bookmark-yank-word bookmark-set bookmark-edit-annotation
;;;;;;  bookmark-send-edited-annotation bookmark-edit-annotation-mode
;;;;;;  bmkp-sort-comparer bmkp-menu-popup-max-length bmkp-incremental-filter-delay
;;;;;;  bmkp-bookmark-name-length-max bmkp-show-end-of-region bmkp-w3m-allow-multi-tabs
;;;;;;  bmkp-su-or-sudo-regexp bmkp-sequence-jump-display-function
;;;;;;  bmkp-handle-region-function bmkp-save-new-location-flag bmkp-region-search-size
;;;;;;  bmkp-other-window-pop-to-flag bmkp-prompt-for-tags-flag bmkp-use-region
;;;;;;  bmkp-this-buffer-cycle-sort-comparer bmkp-desktop-no-save-vars
;;;;;;  bmkp-default-handler-associations bmkp-default-bookmark-name
;;;;;;  bmkp-crosshairs-flag bmkp-autoname-format bmkp-autoname-bookmark-function)
;;;;;;  "bookmark+-1" "bookmark-plus/bookmark+-1.el" (19806 50605))
;;; Generated autoloads from bookmark-plus/bookmark+-1.el

(defvar bmkp-autoname-bookmark-function 'bmkp-autoname-bookmark "\
*Function to automatically name a bookmark at point (cursor position).")

(custom-autoload 'bmkp-autoname-bookmark-function "bookmark+-1" t)

(defvar bmkp-autoname-format (if (> emacs-major-version 21) "^[0-9]\\{9\\} %s" "^[0-9]+ %s") "\
*Format string to match an autonamed bookmark name.
It must have a single `%s' that to accept the buffer name.")

(custom-autoload 'bmkp-autoname-format "bookmark+-1" t)

(defvar bmkp-crosshairs-flag (> emacs-major-version 21) "\
*Non-nil means highlight with crosshairs when you visit a bookmark.
The highlighting is temporary - until your next action.
You need library `crosshairs.el' for this feature, and you need Emacs
22 or later.

If you use this option in Lisp code, you will want to add/remove
`bmkp-crosshairs-highlight' to/from `bookmark-after-jump-hook'.")

(custom-autoload 'bmkp-crosshairs-flag "bookmark+-1" nil)

(defvar bmkp-default-bookmark-name 'highlighted "\
*Default bookmark name preference.
In `*Bookmark List*' use the name of the current line's bookmark.
Otherwise, if `bookmark+-lit.el' is not loaded then use the name of
 the last-used bookmark in the current file.

Otherwise, use this option to determine the default, by preferring one
of the following, if available:

* a highlighted bookmark at point
* the last-used bookmark in the current file")

(custom-autoload 'bmkp-default-bookmark-name "bookmark+-1" t)

(defvar bmkp-default-handler-associations (and (require 'dired-x) (let ((assns nil)) (dolist (shell-assn dired-guess-shell-alist-user) (push (cons (car shell-assn) `(lambda (bmk) (dired-run-shell-command (dired-shell-stuff-it ,(cadr shell-assn) (list (bookmark-get-filename bmk)) nil nil)))) assns)) assns)) "\
File associations for bookmark handlers used for indirect bookmarks.
Each element of the alist is (REGEXP . COMMAND).
REGEXP matches a file name.
COMMAND is a sexp that evaluates to either a shell command (a string)
 or an Emacs function (a symbol or a lambda form).

Example value:

 ((\"\\.pdf$\" . \"AcroRd32.exe\") ; Adobe Acrobat Reader
  (\"\\.ps$\" . \"gsview32.exe\")) ; Ghostview (PostScript viewer)

When you change this option using Customize, if you want to use a
literal string as COMMAND then you must double-quote the text:
\"...\".  (But do not use double-quotes for the REGEXP.)  If you want
to use a symbol as COMMAND, then single-quote it - e.g. 'foo.

This option is used by `bmkp-default-handler-user'.  If an association
for a given file name is not found using this option, then
`bmkp-default-handler-for-file' looks for an association in
`dired-guess-shell-alist-user', `dired-guess-shell-alist-default', and
in mailcap entries (Emacs 23+), in that order.")

(custom-autoload 'bmkp-default-handler-associations "bookmark+-1" t)

(defvar bmkp-desktop-no-save-vars '(search-ring regexp-search-ring kill-ring) "\
List of variables not to save when creating a desktop bookmark.
They are removed from `desktop-globals-to-save' for the duration of
the save (only).")

(custom-autoload 'bmkp-desktop-no-save-vars "bookmark+-1" t)

(defvar bmkp-this-buffer-cycle-sort-comparer '((bmkp-position-cp)) "\
*`bmkp-sort-comparer' value for cycling this-buffer bookmarks.
Some values you might want to use: ((bmkp-position-cp)),
 ((bmkp-bookmark-creation-cp)), ((bmkp-visited-more-cp)).
See `bmkp-sort-comparer'.")

(custom-autoload 'bmkp-this-buffer-cycle-sort-comparer "bookmark+-1" t)

(defvar bmkp-use-region t "\
*Non-nil means visiting a bookmark activates its recorded region.")

(custom-autoload 'bmkp-use-region "bookmark+-1" t)

(defvar bmkp-prompt-for-tags-flag nil "\
*Non-nil means `bookmark-set' prompts for tags (when called interactively).")

(custom-autoload 'bmkp-prompt-for-tags-flag "bookmark+-1" t)

(defvar bmkp-other-window-pop-to-flag t "\
*Non-nil means other-window bookmark jumping uses `pop-to-buffer'.
Use nil if you want the vanilla Emacs behavior, which uses
`switch-to-buffer-other-window'.  That creates a new window even if
there is already another window showing the buffer.")

(custom-autoload 'bmkp-other-window-pop-to-flag "bookmark+-1" t)

(defvar bmkp-region-search-size 40 "\
*Same as `bookmark-search-size', but specialized for bookmark regions.")

(custom-autoload 'bmkp-region-search-size "bookmark+-1" t)

(defvar bmkp-save-new-location-flag t "\
*Non-nil means save automatically relocated bookmarks.
If nil, then the new bookmark location is visited, but it is not saved
as part of the bookmark definition.")

(custom-autoload 'bmkp-save-new-location-flag "bookmark+-1" t)

(defvar bmkp-handle-region-function 'bmkp-handle-region-default "\
*Function to handle a bookmarked region.")

(custom-autoload 'bmkp-handle-region-function "bookmark+-1" t)

(defvar bmkp-sequence-jump-display-function 'pop-to-buffer "\
*Function used to display the bookmarks in a bookmark sequence.")

(custom-autoload 'bmkp-sequence-jump-display-function "bookmark+-1" t)

(defvar bmkp-su-or-sudo-regexp "\\(/su:\\|/sudo:\\)" "\
*Regexp to recognize `su' or `sudo' Tramp bookmarks.")

(custom-autoload 'bmkp-su-or-sudo-regexp "bookmark+-1" t)

(defvar bmkp-w3m-allow-multi-tabs t "\
*Non-nil means jump to W3M bookmarks in a new session.")

(custom-autoload 'bmkp-w3m-allow-multi-tabs "bookmark+-1" t)

(defvar bmkp-show-end-of-region t "\
*Show end of region with `exchange-point-and-mark' when activating a region.
If nil show only beginning of region.")

(custom-autoload 'bmkp-show-end-of-region "bookmark+-1" t)

(defvar bmkp-bookmark-name-length-max 70 "\
*Max number of chars for default name for a bookmark with a region.")

(custom-autoload 'bmkp-bookmark-name-length-max "bookmark+-1" t)

(defvar bmkp-incremental-filter-delay 0.6 "\
*Seconds to wait before updating display when filtering bookmarks.")

(custom-autoload 'bmkp-incremental-filter-delay "bookmark+-1" t)

(defvar bmkp-menu-popup-max-length 20 "\
*Max number of bookmarks for `bookmark-completing-read' to use a menu.
When choosing a bookmark from a list of bookmarks using
`bookmark-completing-read', this controls whether to use a menu or
minibuffer input with completion.
If t, then always use a menu.
If nil, then never use a menu.
If an integer, then use a menu only if there are fewer bookmark
 choices than the value.")

(custom-autoload 'bmkp-menu-popup-max-length "bookmark+-1" t)

(defvar bmkp-sort-comparer '((bmkp-info-cp bmkp-gnus-cp bmkp-url-cp bmkp-local-file-type-cp) bmkp-alpha-p) "\
*Predicate or predicates for sorting (comparing) bookmarks.
This defines the default sort for bookmarks in the bookmark list.

Various sorting commands, such as \\<bookmark-bmenu-mode-map>`\\[bmkp-bmenu-sort-by-bookmark-visit-frequency]', change the value of this
option dynamically (but they do not save the changed value).

The value must be one of the following:

* nil, meaning do not sort

* a predicate that takes two bookmarks as args

* a list of the form ((PRED...) FINAL-PRED), where each PRED and
  FINAL-PRED are predicates that take two bookmarks as args

If the value is a list of predicates, then each PRED is tried in turn
until one returns a non-nil value.  In that case, the result is the
car of that value.  If no non-nil value is returned by any PRED, then
FINAL-PRED is used and its value is the result.

Each PRED should return `(t)' for true, `(nil)' for false, or nil for
undecided.  A nil value means that the next PRED decides (or
FINAL-PRED, if there is no next PRED).

Thus, a PRED is a special kind of predicate that indicates either a
boolean value (as a singleton list) or \"I cannot decide - let the
next guy else decide\".  (Essentially, each PRED is a hook function
that is run using `run-hook-with-args-until-success'.)

Examples:

 nil           - No sorting.

 string-lessp  - Single predicate that returns nil or non-nil.

 ((p1 p2))     - Two predicates `p1' and `p2', which each return
                 (t) for true, (nil) for false, or nil for undecided.

 ((p1 p2) string-lessp)
               - Same as previous, except if both `p1' and `p2' return
                 nil, then the return value of `string-lessp' is used.

Note that these two values are generally equivalent, in terms of their
effect (*):

 ((p1 p2))
 ((p1) p2-plain) where p2-plain is (bmkp-make-plain-predicate p2)

Likewise, these three values generally act equivalently (*):

 ((p1))
 (() p1-plain)
 p1-plain        where p1-plain is (bmkp-make-plain-predicate p1)

The PRED form lets you easily combine predicates: use `p1' unless it
cannot decide, in which case try `p2', and so on.  The value ((p2 p1))
tries the predicates in the opposite order: first `p2', then `p1' if
`p2' returns nil.

Using a single predicate or FINAL-PRED makes it easy to reuse an
existing predicate that returns nil or non-nil.

You can also convert a PRED-type predicate (which returns (t), (nil),
or nil) into an ordinary predicate, by using function
`bmkp-make-plain-predicate'.  That lets you reuse elsewhere, as
ordinary predicates, any PRED-type predicates you define.

For example, this defines a plain predicate to compare by URL:
 (defalias 'bmkp-url-p (bmkp-make-plain-predicate 'bmkp-url-cp))

Note: As a convention, predefined Bookmark+ PRED-type predicate names
have the suffix `-cp' (for \"component predicate\") instead of `-p'.

--
* If you use `\\[bmkp-reverse-multi-sort-order]', then there is a difference in behavior between

   (a) using a plain predicate as FINAL-PRED and
   (b) using the analogous PRED-type predicate (and no FINAL-PRED).

  In the latter case, `\\[bmkp-reverse-multi-sort-order]' affects when the predicate is tried and
  its return value.  See `bmkp-reverse-multi-sort-order'.")

(custom-autoload 'bmkp-sort-comparer "bookmark+-1" t)

(autoload 'bookmark-edit-annotation-mode "bookmark+-1" "\
Mode for editing the annotation of bookmark BOOKMARK.
When you have finished composing, type \\[bookmark-send-annotation].
BOOKMARK is a bookmark name or a bookmark record.

\\{bookmark-edit-annotation-mode-map}

\(fn BOOKMARK)" t nil)

(autoload 'bookmark-send-edited-annotation "bookmark+-1" "\
Use buffer contents as annotation for a bookmark.
Lines beginning with `#' are ignored.

\(fn)" t nil)

(autoload 'bookmark-edit-annotation "bookmark+-1" "\
Pop up a buffer for editing bookmark BOOKMARK's annotation.
BOOKMARK is a bookmark name or a bookmark record.

\(fn BOOKMARK)" t nil)

(autoload 'bookmark-set "bookmark+-1" "\
Set a bookmark named NAME, then run `bmkp-after-set-hook'.
If the region is active (`transient-mark-mode') and nonempty, then
record the region limits in the bookmark.

If NAME is nil, then prompt for the bookmark name.  The default name
for prompting is as follows (in order of priority):

 * If the region is active and nonempty, then the buffer name followed
   by \": \" and the region prefix (up to
   `bmkp-bookmark-name-length-max' chars).

 * If in W3M mode, then the current W3M title.

 * If in a Gnus mode, then the Gnus summary article header.

 * If on a `man' page, then the page name (command and section).

 * Otherwise, the current buffer name.

While entering a bookmark name at the prompt:

 * You can use (lax) completion against bookmarks in the same buffer.
   If there are no bookmarks in the current buffer, then all bookmarks
   are completion candidates.  (See also below, about a numeric prefix
   argument.)

 * You can use `C-w' to yank words from the buffer to the minibuffer.
   Repeating `C-w' yanks successive words.

 * You can use `C-u' to insert the name of the last bookmark used in
   the buffer.  This can be useful as an aid to track your progress
   through a large file.  (If no bookmark has yet been used, then
   `C-u' inserts the name of the visited file.)

A prefix argument changes the behavior as follows:

 * Numeric prefix arg: Use all bookmarks as completion candidates,
   instead of just the bookmarks for the current buffer.

 * Plain prefix arg (`C-u'): Do not overwrite a bookmark that has the
   same name as NAME, if such a bookmark already exists.  Instead,
   push the new bookmark onto the bookmark alist.

   The most recently set bookmark named NAME is thus the one in effect
   at any given time, but any others named NAME are still available,
   should you decide to delete the most recent one.

Use `\\[bookmark-delete]' to remove bookmarks (you give it a name, and it removes
only the first instance of a bookmark with that name from the list of
bookmarks).

\(fn &optional NAME PARG INTERACTIVEP)" t nil)

(autoload 'bookmark-yank-word "bookmark+-1" "\
Yank the word at point in `bookmark-current-buffer'.
Repeat to yank subsequent words from the buffer, appending them.
Newline characters are stripped out.

\(fn)" t nil)

(autoload 'bookmark-jump "bookmark+-1" "\
Jump to bookmark BOOKMARK.
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See function `bookmark-load' for more about this.

If the file pointed to by BOOKMARK no longer exists, you are asked if
you wish to give the bookmark a new location.  If so, `bookmark-jump'
jumps to the new location and saves it.

If the bookmark defines a region, then the region is activated if
`bmkp-use-region' is not-nil or it is nil and you use a prefix
argument.  A prefix arg temporarily flips the value of
`bmkp-use-region'.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate.

In Lisp code:
BOOKMARK is a bookmark name or a bookmark record.
Non-nil DISPLAY-FUNCTION is a function to display the bookmark.  By
 default, `switch-to-buffer' is used.
Non-nil USE-REGION-P flips the value of `bmkp-use-region'.

\(fn BOOKMARK &optional DISPLAY-FUNCTION USE-REGION-P)" t nil)

(autoload 'bookmark-jump-other-window "bookmark+-1" "\
Jump to bookmark BOOKMARK in another window.
See `bookmark-jump', in particular for info about using a prefix arg.

\(fn BOOKMARK &optional USE-REGION-P)" t nil)

(autoload 'bookmark-relocate "bookmark+-1" "\
Relocate the bookmark named BOOKMARK-NAME to another file.
You are prompted for the new file name.
Changes the file associated with the bookmark.
Useful when a file has been renamed after a bookmark was set in it.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate.

\(fn BOOKMARK-NAME)" t nil)

(autoload 'bookmark-insert-location "bookmark+-1" "\
Insert file or buffer name for the bookmark named BOOKMARK-NAME.
If a file is bookmarked, insert the recorded file name.
If a non-file buffer is bookmarked, insert the recorded buffer name.

Optional second arg NO-HISTORY means do not record this in the
minibuffer history list `bookmark-history'.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate.

\(fn BOOKMARK-NAME &optional NO-HISTORY)" t nil)

(autoload 'bookmark-rename "bookmark+-1" "\
Change bookmark's name from OLD to NEW.
Interactively:
 If called from the keyboard, then prompt for OLD.
 If called from the menubar, select OLD from a menu.
If NEW is nil, then prompt for its string value.

If BATCH is non-nil, then do not rebuild the bookmark list.

While the user enters the new name, repeated `C-w' inserts consecutive
words from the buffer into the new bookmark name.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate.

\(fn OLD &optional NEW BATCH)" t nil)

(autoload 'bookmark-insert "bookmark+-1" "\
Insert the text of a bookmarked file.
BOOKMARK-NAME is the name of the bookmark.
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See function `bookmark-load' for more about this.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate.

\(fn BOOKMARK-NAME)" t nil)

(autoload 'bookmark-delete "bookmark+-1" "\
Delete the bookmark named BOOKMARK-NAME from the bookmark list.
Removes only the first instance of a bookmark with that name.
If there are other bookmarks with the same name, they are not deleted.
Defaults to the \"current\" bookmark (that is, the one most recently
used in this file), if it exists.  Optional second arg BATCH means do
not update the bookmark list buffer (probably because we were called
from there).

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate.  In this way, you can delete multiple bookmarks.

\(fn BOOKMARK-NAME &optional BATCH)" t nil)

(autoload 'bookmark-save "bookmark+-1" "\
Save currently defined bookmarks.
Save by default in the file named by variable
`bmkp-current-bookmark-file'.  With a prefix arg, you are prompted for
the file to save to.

To load bookmarks from a specific file, use `\\[bookmark-load]'
\(`bookmark-load').

If called from Lisp:
 Witn nil PARG, use file `bmkp-current-bookmark-file'.
 With non-nil PARG and non-nil FILE, use file FILE.
 With non-nil PARG and nil FILE, prompt the user for the file to use.

\(fn &optional PARG FILE)" t nil)

(autoload 'bookmark-load "bookmark+-1" "\
Load bookmarks from FILE (which must be in the standard format).
Without a prefix argument (argument OVERWRITE is nil), add the newly
loaded bookmarks to those already current.  They will be saved to the
current bookmark file when bookmarks are saved.  If you have never
switched bookmark files, then this is the default file,
`bookmark-default-file'.

If you do not use a prefix argument, then no existing bookmarks are
overwritten.  If you load some bookmarks that have the same names as
bookmarks already defined in your Emacs session, numeric suffixes
\"<2>\", \"<3>\",... are appended as needed to the names of those new
bookmarks to distinguish them.

With a prefix argument, switch the bookmark file currently used,
*replacing* all currently existing bookmarks with the newly loaded
bookmarks.  The value of `bmkp-current-bookmark-file' is changed to
FILE, so bookmarks will subsequently be saved to FILE.  The value
`bookmark-default-file' is unaffected, so your next Emacs session will
still use the same default set of bookmarks.

When called from Lisp, non-nil NO-MSG means do not display any
messages while loading.

You do not need to manually load your default bookmark file
\(`bookmark-default-file') - it is loaded automatically by Emacs the
first time you use bookmarks in a session.  Use `bookmark-load' only
to load extra bookmarks (with no prefix arg) or an alternative set of
bookmarks (with a prefix arg).

If you use `bookmark-load' to load a file that does not contain a
proper bookmark alist, then when bookmarks are saved the current
bookmark file will likely become corrupted.  You should load only
bookmark files that were created using the bookmark functions.

\(fn FILE &optional OVERWRITE NO-MSG)" t nil)

(autoload 'bmkp-edit-bookmark "bookmark+-1" "\
Edit BOOKMARK's name and file name, and maybe save them.
With a prefix argument, edit the complete bookmark record (the
internal, Lisp form).

BOOKMARK is a bookmark name (a string) or a bookmark record.

\(fn BOOKMARK &optional INTERNALP)" t nil)

(autoload 'bmkp-edit-bookmark-record "bookmark+-1" "\
Edit the internal record for bookmark BOOKMARK.
When you have finished, Use `\\[bmkp-edit-bookmark-record-send]'.
BOOKMARK is a bookmark name (a string) or a bookmark record.

\(fn BOOKMARK)" t nil)

(autoload 'bmkp-edit-bookmark-record-send "bookmark+-1" "\
Use buffer contents as a bookmark record.
Lines beginning with `#;' are ignored.
With a prefix argument, do not update `time' or `visits' entries.

\(fn ARG &optional FORCE)" t nil)

(autoload 'bmkp-send-bug-report "bookmark+-1" "\
Send a bug report about a Bookmark+ problem.

\(fn)" t nil)

(autoload 'bmkp-toggle-bookmark-set-refreshes "bookmark+-1" "\
Toggle `bookmark-set' refreshing `bmkp-latest-bookmark-alist'.
Add/remove `bmkp-refresh-latest-bookmark-list' to/from
`bmkp-after-set-hook'.

\(fn)" t nil)

(autoload 'bmkp-toggle-saving-menu-list-state "bookmark+-1" "\
Toggle the value of option `bmkp-bmenu-state-file'.
Tip: You can use this before quitting Emacs, to not save the state.
If the initial value of `bmkp-bmenu-state-file' is nil, then this
command has no effect.

\(fn)" t nil)

(autoload 'bmkp-toggle-saving-bookmark-file "bookmark+-1" "\
Toggle the value of option `bookmark-save-flag'.
If the initial value of `bookmark-save-flag' is nil, then this
command has no effect.

\(fn)" t nil)

(autoload 'bmkp-make-function-bookmark "bookmark+-1" "\
Create a bookmark that invokes FUNCTION when \"jumped\" to.
You are prompted for the bookmark name and the name of the function.
Returns the new bookmark (internal record).

\(fn BOOKMARK-NAME FUNCTION)" t nil)

(autoload 'bmkp-switch-bookmark-file "bookmark+-1" "\
Switch to a different bookmark file, FILE.
Replace all currently existing bookmarks with the newly loaded
bookmarks.  Change the value of `bmkp-current-bookmark-file' to FILE,
so bookmarks will subsequently be saved to FILE.

`bookmark-default-file' is unaffected, so your next Emacs session will
still use `bookmark-default-file' for the initial set of bookmarks.

\(fn FILE &optional NO-MSG)" t nil)

(autoload 'bmkp-switch-to-last-bookmark-file "bookmark+-1" "\
Switch back to the last-used bookmarkfile.
Replace all currently existing bookmarks with those newly loaded from
the last-used file.  Swap the values of `bmkp-last-bookmark-file' and
`bmkp-current-bookmark-file'.

\(fn &optional NO-MSG)" t nil)

(autoload 'bmkp-use-bookmark-file-create "bookmark+-1" "\
Switch current bookmark file to FILE, creating it if it does not exist.
Interactively, you are prompted for the file name.  The default is
`.emacs.bmk' in the current directory, but you can enter any file
name, anywhere.

If there is no file with the name you provide then a new, an empty
bookmark file with that name is created.

You are prompted to confirm the bookmark-file switch.

Returns FILE.

\(fn FILE)" t nil)

(autoload 'bmkp-empty-file "bookmark+-1" "\
Empty the bookmark file FILE, or create FILE (empty) if it does not exist.
In either case, FILE will become an empty bookmark file.  Return FILE.

NOTE: If FILE already exists and you confirm emptying it, no check is
      made that it is in fact a bookmark file before emptying it.
      It is simply replaced by an empty bookmark file and saved.

This does NOT also make FILE the current bookmark file.  To do that,
use `\\[bmkp-switch-bookmark-file]' (`bmkp-switch-bookmark-file').

\(fn FILE)" t nil)

(autoload 'bmkp-crosshairs-highlight "bookmark+-1" "\
Call `crosshairs-highlight', unless the region is active.
You can add this to hook `bookmark-after-jump-hook'.
You need library `crosshairs.el' to use this command.

\(fn)" t nil)

(autoload 'bmkp-choose-navlist-from-bookmark-list "bookmark+-1" "\
Choose a bookmark-list bookmark and set the bookmark navigation list.
The navigation-list variable, `bmkp-nav-alist', is set to the list of
bookmarks that would be displayed by `bookmark-bmenu-list' (`C-x r l')
for the chosen bookmark-list bookmark, sorted and filtered as
appropriate.

Instead of choosing a bookmark-list bookmark, you can choose the
pseudo-bookmark `CURRENT *Bookmark List*'.  The bookmarks used for the
navigation list are those that would be currently shown in the
`*Bookmark List*' (even if the list is not currently displayed).

\(fn BOOKMARK-NAME &optional ALIST)" t nil)

(autoload 'bmkp-choose-navlist-of-type "bookmark+-1" "\
Set the bookmark navigation list to the bookmarks of a type you choose.
The pseudo-type `any' sets the navigation list to all bookmarks.
This sets variable `bmkp-nav-alist'.

\(fn TYPE)" t nil)

(autoload 'bmkp-this-buffer-bmenu-list "bookmark+-1" "\
Show the bookmark list just for bookmarks for the current buffer.
Set `bmkp-last-specific-buffer' to the current buffer name.

\(fn)" t nil)

(autoload 'bmkp-navlist-bmenu-list "bookmark+-1" "\
Show the bookmark list just for bookmarks from the navigation list.

\(fn)" t nil)

(autoload 'bmkp-unomit-all "bookmark+-1" "\
Remove all bookmarks from the list of omitted bookmarks.
All bookmarks will henceforth be available for display.

\(fn)" t nil)

(autoload 'bmkp-list-all-tags "bookmark+-1" "\
List all tags used for any bookmarks.
With a prefix argument, list the full alist of tags.
Otherwise, list only the tag names.

Note that when the full alist is shown, the same tag name will appear
once for each of its different values.

Show list in minibuffer or, if not enough space, buffer `*All Tags*'.

\(fn FULLP)" t nil)

(autoload 'bmkp-remove-all-tags "bookmark+-1" "\
Remove all tags from BOOKMARK.
Non-nil optional arg MSGP means display a message about the removal.

\(fn BOOKMARK &optional MSGP)" t nil)

(autoload 'bmkp-add-tags "bookmark+-1" "\
Add TAGS to BOOKMARK.
Hit `RET' to enter each tag, then hit `RET' again after the last tag.
You can use completion to enter the bookmark name and each tag.
Completion for the bookmark name is strict.
Completion for tags is lax: you are not limited to existing tags.

TAGS is a list of strings.
Non-nil MSGP means display a message about the addition.
Non-nil NO-CACHE-UPDATE-P means do not update `bmkp-tags-alist'.
Return the number of tags added.

\(fn BOOKMARK TAGS &optional MSGP NO-CACHE-UPDATE-P)" t nil)

(autoload 'bmkp-set-tag-value-for-navlist "bookmark+-1" "\
Set the value of TAG to VALUE, for each bookmark in the navlist.
If any of the bookmarks has no tag named TAG, then add one with VALUE.

\(fn TAG VALUE)" t nil)

(autoload 'bmkp-set-tag-value "bookmark+-1" "\
For BOOKMARK's TAG, set the value to VALUE.
If BOOKMARK has no tag named TAG, then add one with value VALUE.

\(fn BOOKMARK TAG VALUE &optional MSGP)" t nil)

(autoload 'bmkp-remove-tags "bookmark+-1" "\
Remove TAGS from BOOKMARK.
Hit `RET' to enter each tag, then hit `RET' again after the last tag.
You can use completion to enter the bookmark name and each tag.

TAGS is a list of strings.  The corresponding tags are removed.
Non-nil MSGP means display messages.
Non-nil NO-CACHE-UPDATE-P means do not update `bmkp-tags-alist'.

\(fn BOOKMARK TAGS &optional MSGP NO-CACHE-UPDATE-P)" t nil)

(autoload 'bmkp-remove-tags-from-all "bookmark+-1" "\
Remove TAGS from all bookmarks.
Hit `RET' to enter each tag, then hit `RET' again after the last tag.
You can use completion to enter each tag.
This affects all bookmarks, even those not showing in bookmark list.

TAGS is a list of strings.  The corresponding tags are removed.
Non-nil optional arg MSGP means display a message about the deletion.

\(fn TAGS &optional MSGP)" t nil)

(autoload 'bmkp-rename-tag "bookmark+-1" "\
Rename TAG to NEWNAME in all bookmarks, even those not showing.
Non-nil optional arg MSGP means display a message about the deletion.

\(fn TAG NEWNAME &optional MSGP)" t nil)

(autoload 'bmkp-url-target-set "bookmark+-1" "\
Set a bookmark for a URL.
Interactively you are prompted for the URL.  Completion is available.
Use `M-n' to pick up the url at point as the default.

You are also prompted for the bookmark name.  But with a prefix arg,
you are prompted only for a bookmark-name prefix.  In that case, the
bookmark name is the prefix followed by the URL.

\(fn URL &optional PREFIX-ONLY-P NAME)" t nil)

(autoload 'bmkp-file-target-set "bookmark+-1" "\
Set a bookmark for FILE.
Interactively you are prompted for FILE.  Completion is available.
Use `M-n' to pick up the file name at point as the default.

You are also prompted for the bookmark name.  But with a prefix arg,
you are prompted only for a bookmark-name prefix.  In that case, the
bookmark name is the prefix followed by the non-directory part of
FILE.

\(fn FILE &optional PREFIX-ONLY-P NAME)" t nil)

(autoload 'bmkp-describe-bookmark "bookmark+-1" "\
Describe BOOKMARK.
With a prefix argument, show the internal definition of the bookmark.
BOOKMARK is a bookmark name or a bookmark record.

\(fn BOOKMARK &optional DEFN)" t nil)

(autoload 'bmkp-describe-bookmark-internals "bookmark+-1" "\
Show the internal definition of the bookmark BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record.

\(fn BOOKMARK)" t nil)

(autoload 'bmkp-list-defuns-in-commands-file "bookmark+-1" "\
List the functions defined in `bmkp-bmenu-commands-file'.
Typically, these are all commands.

\(fn)" t nil)

(autoload 'bmkp-set-bookmark-file-bookmark "bookmark+-1" "\
Create a bookmark that loads bookmark-file FILE when \"jumped\" to.
You are prompted for the names of the bookmark file and the bookmark.

\(fn FILE &optional MSGP)" t nil)

(autoload 'bmkp-bookmark-file-jump "bookmark+-1" "\
Jump to a bookmark-file bookmark, which means load its bookmark file.
With a prefix argument, switch to the new bookmark file.
Otherwise, load it to supplement the current bookmark list.

\(fn BOOKMARK-NAME &optional SWITCHP NO-MSG)" t nil)

(autoload 'bmkp-set-desktop-bookmark "bookmark+-1" "\
Save the desktop as a bookmark.
You are prompted for the desktop-file location and the bookmark name.

\(fn DESKTOP-FILE)" t nil)

(autoload 'bmkp-desktop-change-dir "bookmark+-1" "\
Change to desktop saved in DESKTOP-FILE.
Kill the desktop as specified by variables `desktop-save-mode' and
 `desktop-save' (starting with Emacs 22).
Clear the desktop and load DESKTOP-FILE DIRNAME.

\(fn DESKTOP-FILE)" t nil)

(autoload 'bmkp-desktop-read "bookmark+-1" "\
Load desktop-file FILE, then run `desktop-after-read-hook'.
Return t if FILE was loaded, nil otherwise.

\(fn FILE)" t nil)

(autoload 'bmkp-desktop-delete "bookmark+-1" "\
Delete desktop bookmark BOOKMARK-NAME, and delete its desktop file.
Release the lock file in that desktop's directory.
\(This means that if you currently have locked a different desktop
in the same directory, then you will need to relock it.)

\(fn BOOKMARK-NAME)" t nil)

(autoload 'bmkp-set-variable-list-bookmark "bookmark+-1" "\
Save a list of variables as a bookmark.
Interactively, read the variables to save, using
`bmkp-read-variables-completing'.

\(fn VARIABLES)" t nil)

(autoload 'bmkp-dired-subdirs "bookmark+-1" "\
Alist of inserted subdirectories, without their positions (markers).
This is like `dired-subdir-alist' but without the top-level dir and
without subdir positions (markers).

\(fn)" t nil)

(autoload 'bmkp-jump-to-type "bookmark+-1" "\
Jump to a bookmark of a given type.  You are prompted for the type.
Otherwise, this is the same as `bookmark-jump' - see that, in
particular, for info about using a prefix argument.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-jump-to-type-other-window "bookmark+-1" "\
Jump to a bookmark of a given type.  You are prompted for the type.
See `bmkp-jump-to-type'.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-autonamed-jump "bookmark+-1" "\
Jump to an autonamed bookmark.
This is a specialization of `bookmark-jump'.

\(fn BOOKMARK-NAME)" t nil)

(autoload 'bmkp-autonamed-jump-other-window "bookmark+-1" "\
Jump to a autonamed bookmark in another window.
See `bmkp-autonamed-jump'.

\(fn BOOKMARK-NAME)" t nil)

(autoload 'bmkp-autonamed-this-buffer-jump "bookmark+-1" "\
Jump to an autonamed bookmark in the current buffer.
This is a specialization of `bookmark-jump'.

\(fn BOOKMARK-NAME)" t nil)

(autoload 'bmkp-autonamed-this-buffer-jump-other-window "bookmark+-1" "\
Jump to a autonamed bookmark in the current buffer, in another window.
See `bmkp-autonamed-jump'.

\(fn BOOKMARK-NAME)" t nil)

(autoload 'bmkp-bookmark-list-jump "bookmark+-1" "\
Jump to a bookmark-list bookmark.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-desktop-jump "bookmark+-1" "\
Jump to a desktop bookmark.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-dired-jump "bookmark+-1" "\
Jump to a Dired bookmark.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-dired-jump-other-window "bookmark+-1" "\
Jump to a Dired bookmark in another window.
See `bmkp-dired-jump'.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-dired-jump-current "bookmark+-1" "\
Jump to a Dired bookmark for the current directory.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-dired-jump-current-other-window "bookmark+-1" "\
Jump to a Dired bookmark for the current directory in another window.
See `bmkp-dired-jump-current'.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-file-jump "bookmark+-1" "\
Jump to a file or directory bookmark.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-file-jump-other-window "bookmark+-1" "\
Jump to a file or directory bookmark in another window.
See `bmkp-file-jump'.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-gnus-jump "bookmark+-1" "\
Jump to a Gnus bookmark.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-gnus-jump-other-window "bookmark+-1" "\
Jump to a Gnus bookmark in another window.
See `bmkp-gnus-jump'.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-info-jump "bookmark+-1" "\
Jump to an Info bookmark.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-info-jump-other-window "bookmark+-1" "\
Jump to an Info bookmark in another window.
See `bmkp-info-jump'.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-local-file-jump "bookmark+-1" "\
Jump to a local file or directory bookmark.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-local-file-jump-other-window "bookmark+-1" "\
Jump to a local file or directory bookmark in another window.
See `bmkp-local-file-jump'.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-man-jump "bookmark+-1" "\
Jump to a `man'-page bookmark.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-man-jump-other-window "bookmark+-1" "\
Jump to a `man'-page bookmark in another window.
See `bmkp-man-jump'.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-non-file-jump "bookmark+-1" "\
Jump to a non-file (buffer) bookmark.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-non-file-jump-other-window "bookmark+-1" "\
Jump to a non-file (buffer) bookmark in another window.
See `bmkp-non-file-jump'.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-region-jump "bookmark+-1" "\
Jump to a region bookmark.
This is a specialization of `bookmark-jump', but without a prefix arg.

\(fn BOOKMARK-NAME)" t nil)

(autoload 'bmkp-region-jump-other-window "bookmark+-1" "\
Jump to a region bookmark in another window.
See `bmkp-region-jump'.

\(fn BOOKMARK-NAME)" t nil)

(autoload 'bmkp-remote-file-jump "bookmark+-1" "\
Jump to a remote file or directory bookmark.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-remote-file-jump-other-window "bookmark+-1" "\
Jump to a remote file or directory bookmark in another window.
See `bmkp-remote-file-jump'.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-specific-buffers-jump "bookmark+-1" "\
Jump to a bookmark for a buffer in list BUFFERS.
Interactively, read buffer names and bookmark name, with completion.

This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument.

\(fn BUFFERS BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-specific-buffers-jump-other-window "bookmark+-1" "\
Jump to a bookmark for a buffer in list BUFFERS in another window.
See `bmkp-specific-buffers-jump'.

\(fn BUFFERS BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-specific-files-jump "bookmark+-1" "\
Jump to a bookmark for a file in list FILES.
Interactively, read file names and bookmark name, with completion.

This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument.

\(fn FILES BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-specific-files-jump-other-window "bookmark+-1" "\
Jump to a bookmark for a buffer in list BUFFERS in another window.
See `bmkp-specific-buffers-jump'.

\(fn FILES BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-this-buffer-jump "bookmark+-1" "\
Jump to a bookmark for the current buffer.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-this-buffer-jump-other-window "bookmark+-1" "\
Jump to a bookmark for the current buffer in another window.
See `bmkp-this-buffer-jump'.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-variable-list-jump "bookmark+-1" "\
Jump to a variable-list bookmark.
This is a specialization of `bookmark-jump'.

\(fn BOOKMARK-NAME)" t nil)

(autoload 'bmkp-url-jump "bookmark+-1" "\
Jump to a URL bookmark.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-url-jump-other-window "bookmark+-1" "\
Jump to an URL bookmark in another window.
See `bmkp-url-jump'.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-w3m-jump "bookmark+-1" "\
Jump to a W3M bookmark.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-w3m-jump-other-window "bookmark+-1" "\
Jump to an W3M bookmark in another window.
See `bmkp-w3m-jump'.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-all-tags-jump "bookmark+-1" "\
Jump to a BOOKMARK that has all of the TAGS.
Hit `RET' to enter each tag, then hit `RET' again after the last tag.
You can use completion to enter the bookmark name and each tag.
If you specify no tags, then every bookmark that has some tags is a
candidate.

\(fn TAGS BOOKMARK)" t nil)

(autoload 'bmkp-all-tags-jump-other-window "bookmark+-1" "\
Jump to a BOOKMARK that has all of the TAGS, in another window.
Hit `RET' to enter each tag, then hit `RET' again after the last tag.
You can use completion to enter the bookmark name and each tag.
If you specify no tags, then every bookmark that has some tags is a
candidate.

\(fn TAGS BOOKMARK)" t nil)

(autoload 'bmkp-all-tags-regexp-jump "bookmark+-1" "\
Jump to a BOOKMARK that has each tag matching REGEXP.
You are prompted for the REGEXP.
Then you are prompted for the BOOKMARK (with completion).

\(fn REGEXP BOOKMARK)" t nil)

(autoload 'bmkp-all-tags-regexp-jump-other-window "bookmark+-1" "\
Jump to a BOOKMARK that has each tag matching REGEXP, in another window.
You are prompted for the REGEXP.
Then you are prompted for the BOOKMARK (with completion).

\(fn REGEXP BOOKMARK)" t nil)

(autoload 'bmkp-some-tags-jump "bookmark+-1" "\
Jump to a BOOKMARK that has at least one of the TAGS.
Hit `RET' to enter each tag, then hit `RET' again after the last tag.
You can use completion to enter the bookmark name and each tag.

\(fn TAGS BOOKMARK)" t nil)

(autoload 'bmkp-some-tags-jump-other-window "bookmark+-1" "\
Jump to a BOOKMARK that has at least one of the TAGS, in another window.
Hit `RET' to enter each tag, then hit `RET' again after the last tag.
You can use completion to enter the bookmark name and each tag.

\(fn TAGS BOOKMARK)" t nil)

(autoload 'bmkp-some-tags-regexp-jump "bookmark+-1" "\
Jump to a BOOKMARK that has a tag matching REGEXP.
You are prompted for the REGEXP.
Then you are prompted for the BOOKMARK (with completion).

\(fn REGEXP BOOKMARK)" t nil)

(autoload 'bmkp-some-tags-regexp-jump-other-window "bookmark+-1" "\
Jump to a BOOKMARK that has a tag matching REGEXP, in another window.
You are prompted for the REGEXP.
Then you are prompted for the BOOKMARK (with completion).

\(fn REGEXP BOOKMARK)" t nil)

(autoload 'bmkp-jump-in-navlist "bookmark+-1" "\
Jump to a bookmark, choosing from those in the navigation list.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-jump-in-navlist-other-window "bookmark+-1" "\
Same as `bmkp-jump-in-navlist', but use another window.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-cycle "bookmark+-1" "\
Cycle through bookmarks in the navlist by INCREMENT (default: 1).
Positive INCREMENT cycles forward.  Negative INCREMENT cycles backward.
Interactively, the prefix arg determines INCREMENT:
 Plain `C-u': 1
 otherwise: the numeric prefix arg value

Plain `C-u' also means start over at first bookmark.

You can set the navigation list using commands
 `bmkp-choose-navlist-from-bookmark-list' and
 `bmkp-choose-navlist-of-type'.

You can cycle among bookmarks in the current buffer using
 `bmkp-cycle-this-buffer' and
 `bmkp-cycle-this-buffer-other-window.'

In Lisp code:
 Non-nil OTHER-WINDOW means jump to the bookmark in another window.
 Non-nil STARTOVERP means reset `bmkp-current-nav-bookmark' to the
 first bookmark in the navlist.

\(fn INCREMENT &optional OTHER-WINDOW STARTOVERP)" t nil)

(autoload 'bmkp-cycle-other-window "bookmark+-1" "\
Same as `bmkp-cycle' but uses another window.

\(fn INCREMENT &optional STARTOVERP)" t nil)

(autoload 'bmkp-cycle-this-buffer "bookmark+-1" "\
Cycle through bookmarks in this buffer by INCREMENT (default: 1).
Positive INCREMENT cycles forward.  Negative INCREMENT cycles backward.
Interactively, the prefix arg determines INCREMENT:
 Plain `C-u': 1
 otherwise: the numeric prefix arg value 

Plain `C-u' also means start over at first bookmark.

You can cycle among bookmarks beyond the current buffer using
`bmkp-cycle' and `bmkp-cycle-other-window.'

You can set your preferred sort order for this-buffer bookmarks by
customizing option `bmkp-this-buffer-cycle-sort-comparer'.

To change the sort order without customizing, you can use `\\[bmkp-this-buffer-bmenu-list]' to
show the `*Bookmark List*' with only this buffer's bookmarks, sort
them there, and use `\\[bmkp-choose-navlist-from-bookmark-list]', choosing `CURRENT *Bookmark List*' as
the navigation list.

Then you can cycle the bookmarks using `bmkp-cycle'
\(`\\[bmkp-next-bookmark-repeat]' etc.), instead of `bmkp-cycle-this-buffer'.

In Lisp code:
 Non-nil OTHER-WINDOW means jump to the bookmark in another window.
 Non-nil STARTOVERP means reset `bmkp-current-nav-bookmark' to the
 first bookmark in the navlist.

\(fn INCREMENT &optional OTHER-WINDOW STARTOVERP)" t nil)

(autoload 'bmkp-cycle-this-buffer-other-window "bookmark+-1" "\
Same as `bmkp-cycle-this-buffer' but use other window.

\(fn INCREMENT &optional STARTOVERP)" t nil)

(autoload 'bmkp-next-bookmark "bookmark+-1" "\
Jump to the Nth next bookmark in the bookmark navigation list.
N defaults to 1, meaning the next bookmark.
Plain `C-u' means start over at first bookmark.
See also `bmkp-cycle'.

\(fn N &optional STARTOVERP)" t nil)

(autoload 'bmkp-previous-bookmark "bookmark+-1" "\
Jump to the Nth previous bookmark in the bookmark navigation list.
See `bmkp-next-bookmark'.

\(fn N &optional STARTOVERP)" t nil)

(autoload 'bmkp-next-bookmark-repeat "bookmark+-1" "\
Jump to the Nth-next bookmark in the bookmark navigation list.
This is a repeatable version of `bmkp-next-bookmark'.
N defaults to 1, meaning the next bookmark.
Plain `C-u' means start over at the first bookmark (and no repeat).

\(fn ARG)" t nil)

(autoload 'bmkp-previous-bookmark-repeat "bookmark+-1" "\
Jump to the Nth-previous bookmark in the bookmark navigation list.
See `bmkp-next-bookmark-repeat'.

\(fn ARG)" t nil)

(autoload 'bmkp-next-bookmark-this-buffer "bookmark+-1" "\
Jump to the Nth-next bookmark in the current buffer.
N defaults to 1, meaning the next one.
Plain `C-u' means start over at the first one.
See also `bmkp-cycle-this-buffer'.

\(fn N &optional STARTOVERP)" t nil)

(autoload 'bmkp-previous-bookmark-this-buffer "bookmark+-1" "\
Jump to the Nth-previous bookmark in the current buffer.
See `bmkp-next-bookmark-this-buffer'.

\(fn N &optional STARTOVERP)" t nil)

(autoload 'bmkp-next-bookmark-this-buffer-repeat "bookmark+-1" "\
Jump to the Nth next bookmark in the current buffer.
This is a repeatable version of `bmkp-next-bookmark-this-buffer'.
N defaults to 1, meaning the next one.
Plain `C-u' means start over at the first one (and no repeat).

\(fn ARG)" t nil)

(autoload 'bmkp-previous-bookmark-this-buffer-repeat "bookmark+-1" "\
Jump to the Nth previous bookmark in the current buffer.
See `bmkp-next-bookmark-this-buffer-repeat'.

\(fn ARG)" t nil)

(autoload 'bmkp-next-bookmark-w32 "bookmark+-1" "\
Windows `Open' the Nth next bookmark in the bookmark navigation list.
MS Windows only.  Invokes the program associated with the file type.
N defaults to 1, meaning the next one.
Plain `C-u' means start over at the first one.
See also `bmkp-cycle'.

\(fn N &optional STARTOVERP)" t nil)

(autoload 'bmkp-previous-bookmark-w32 "bookmark+-1" "\
Windows `Open' the Nth previous bookmark in the bookmark navlist.
See `bmkp-next-bookmark-w32'.

\(fn N &optional STARTOVERP)" t nil)

(autoload 'bmkp-next-bookmark-w32-repeat "bookmark+-1" "\
Windows `Open' the Nth next bookmark in the bookmark navigation list.
This is a repeatable version of `bmkp-next-bookmark'.
N defaults to 1, meaning the next bookmark.
Plain `C-u' means start over at the first one (and no repeat).

\(fn ARG)" t nil)

(autoload 'bmkp-previous-bookmark-w32-repeat "bookmark+-1" "\
Windows `Open' the Nth previous bookmark in the bookmark navlist.
See `bmkp-next-bookmark-w32-repeat'.

\(fn ARG)" t nil)

(autoload 'bmkp-toggle-autonamed-bookmark-set/delete "bookmark+-1" "\
If there is an autonamed bookmark at point, delete it, else create one.
The bookmark created has no region.  Its name is formatted according
to option `bmkp-autoname-bookmark-function'.

With a prefix arg, delete *ALL* autonamed bookmarks for this buffer.

Non-interactively, act at POSITION, not point.

\(fn POSITION &optional ALLP)" t nil)

(autoload 'bmkp-set-autonamed-bookmark "bookmark+-1" "\
Set an autonamed bookmark at point.
The bookmark created has no region.  Its name is formatted according
to option `bmkp-autoname-bookmark-function'.
Non-interactively, act at POSITION, not point.

\(fn POSITION &optional MSGP)" t nil)

(autoload 'bmkp-set-autonamed-bookmark-at-line "bookmark+-1" "\
Set an autonamed bookmark at the beginning of the given line NUMBER.

\(fn NUMBER)" t nil)

(autoload 'bmkp-set-autonamed-regexp-buffer "bookmark+-1" "\
Set autonamed bookmarks at matches for REGEXP in the buffer.

\(fn REGEXP &optional MSGP)" t nil)

(autoload 'bmkp-set-autonamed-regexp-region "bookmark+-1" "\
Set autonamed bookmarks at matches for REGEXP in the region.

\(fn REGEXP BEG END &optional MSGP)" t nil)

(autoload 'bmkp-delete-all-autonamed-for-this-buffer "bookmark+-1" "\
Delete all autonamed bookmarks for the current buffer.
To be deleted, a bookmark name must be an autonamed bookmark whose
buffer part names the current buffer.

\(fn)" t nil)

(autoload 'bmkp-delete-bookmarks "bookmark+-1" "\
Delete some bookmarks at point or all bookmarks in the buffer.
With no prefix argument, delete some bookmarks at point.
If there is more than one, require confirmation for each.

With a prefix argument, delete *ALL* bookmarks in the current buffer.

Non-interactively, delete at POSITION.
Optional arg ALIST is the alist of bookmarks.  It defaults to
`bookmark-alist'.

\(fn POSITION ALLP &optional ALIST)" t nil)

;;;***

;;;### (autoloads (bmkp-bmenu-mouse-3-menu bmkp-bmenu-describe-marked
;;;;;;  bmkp-bmenu-describe-this-bookmark bmkp-bmenu-describe-this+move-up
;;;;;;  bmkp-bmenu-describe-this+move-down bmkp-reverse-multi-sort-order
;;;;;;  bmkp-reverse-sort-order bmkp-bmenu-change-sort-order bmkp-bmenu-change-sort-order-repeat
;;;;;;  bmkp-bmenu-quit bmkp-bmenu-edit-bookmark bmkp-define-tags-sort-command
;;;;;;  bmkp-bmenu-define-full-snapshot-command bmkp-bmenu-define-command
;;;;;;  bmkp-bmenu-define-jump-marked-command bmkp-bmenu-mode-status-help
;;;;;;  bmkp-bmenu-w32-open-select bmkp-bmenu-w32-open-with-mouse
;;;;;;  bmkp-bmenu-w32-open bmkp-bmenu-unmark-bookmarks-tagged-not-all
;;;;;;  bmkp-bmenu-unmark-bookmarks-tagged-some bmkp-bmenu-unmark-bookmarks-tagged-none
;;;;;;  bmkp-bmenu-unmark-bookmarks-tagged-all bmkp-bmenu-mark-bookmarks-tagged-not-all
;;;;;;  bmkp-bmenu-mark-bookmarks-tagged-some bmkp-bmenu-mark-bookmarks-tagged-none
;;;;;;  bmkp-bmenu-mark-bookmarks-tagged-all bmkp-bmenu-mark-bookmarks-tagged-regexp
;;;;;;  bmkp-bmenu-remove-tags-from-marked bmkp-bmenu-add-tags-to-marked
;;;;;;  bmkp-bmenu-remove-tags bmkp-bmenu-set-tag-value-for-marked
;;;;;;  bmkp-bmenu-set-tag-value bmkp-bmenu-add-tags bmkp-bmenu-remove-all-tags
;;;;;;  bmkp-bmenu-show-only-tagged bmkp-bmenu-query-replace-marked-bookmarks-regexp
;;;;;;  bmkp-bmenu-search-marked-bookmarks-regexp bmkp-bmenu-show-only-omitted
;;;;;;  bmkp-bmenu-unomit-marked bmkp-bmenu-omit-marked bmkp-bmenu-omit/unomit-marked
;;;;;;  bmkp-bmenu-omit bmkp-bmenu-make-sequence-from-marked bmkp-bmenu-delete-marked
;;;;;;  bmkp-bmenu-dired-marked bmkp-bmenu-toggle-marks bmkp-bmenu-mark-bookmarks-satisfying
;;;;;;  bmkp-bmenu-mark-w3m-bookmarks bmkp-bmenu-mark-url-bookmarks
;;;;;;  bmkp-bmenu-mark-specific-file-bookmarks bmkp-bmenu-mark-specific-buffer-bookmarks
;;;;;;  bmkp-bmenu-mark-region-bookmarks bmkp-bmenu-mark-non-file-bookmarks
;;;;;;  bmkp-bmenu-mark-man-bookmarks bmkp-bmenu-mark-info-bookmarks
;;;;;;  bmkp-bmenu-mark-gnus-bookmarks bmkp-bmenu-mark-file-bookmarks
;;;;;;  bmkp-bmenu-mark-dired-bookmarks bmkp-bmenu-mark-desktop-bookmarks
;;;;;;  bmkp-bmenu-mark-bookmark-file-bookmarks bmkp-bmenu-regexp-mark
;;;;;;  bmkp-bmenu-unmark-all bmkp-bmenu-mark-all bmkp-bmenu-toggle-show-only-marked
;;;;;;  bmkp-bmenu-toggle-show-only-unmarked bmkp-bmenu-filter-tags-incrementally
;;;;;;  bmkp-bmenu-filter-annotation-incrementally bmkp-bmenu-filter-file-name-incrementally
;;;;;;  bmkp-bmenu-filter-bookmark-name-incrementally bmkp-bmenu-refresh-menu-list
;;;;;;  bmkp-bmenu-show-all bmkp-bmenu-show-only-w3m-urls bmkp-bmenu-show-only-urls
;;;;;;  bmkp-bmenu-show-only-specific-file bmkp-bmenu-show-only-specific-buffer
;;;;;;  bmkp-bmenu-show-only-variable-lists bmkp-bmenu-show-only-regions
;;;;;;  bmkp-bmenu-show-only-man-pages bmkp-bmenu-show-only-info-nodes
;;;;;;  bmkp-bmenu-show-only-gnus bmkp-bmenu-show-only-non-files
;;;;;;  bmkp-bmenu-show-only-files bmkp-bmenu-show-only-dired bmkp-bmenu-show-only-desktops
;;;;;;  bmkp-bmenu-show-only-bookmark-files bmkp-bmenu-show-only-autonamed
;;;;;;  bookmark-bmenu-rename bookmark-bmenu-execute-deletions bookmark-bmenu-show-annotation
;;;;;;  bookmark-bmenu-other-window-with-mouse bookmark-bmenu-switch-other-window
;;;;;;  bookmark-bmenu-other-window bookmark-bmenu-this-window bookmark-bmenu-2-window
;;;;;;  bookmark-bmenu-1-window bookmark-bmenu-list bookmark-bmenu-delete
;;;;;;  bookmark-bmenu-unmark bookmark-bmenu-mark bmkp-bmenu-state-file
;;;;;;  bmkp-bmenu-commands-file bmkp-bmenu-omitted-list) "bookmark+-bmu"
;;;;;;  "bookmark-plus/bookmark+-bmu.el" (19804 63219))
;;; Generated autoloads from bookmark-plus/bookmark+-bmu.el

(defvar bmkp-bmenu-omitted-list nil "\
List of names of omitted bookmarks.
They are generally not available for display in the bookmark list.
You can, however, use \\<bookmark-bmenu-mode-map>`\\[bmkp-bmenu-show-only-omitted]' to see them.
You can then mark some of them and use `\\[bmkp-bmenu-omit/unomit-marked]'
 to make those that are marked available again for the bookmark list.")

(custom-autoload 'bmkp-bmenu-omitted-list "bookmark+-bmu" t)

(defvar bmkp-bmenu-commands-file (convert-standard-filename "~/.emacs-bmk-bmenu-commands.el") "\
*File for saving user-defined bookmark-list commands.
This must be an absolute file name (possibly containing `~') or nil
\(it is not expanded).

You can use `\\[bmkp-list-defuns-in-commands-file]' to list the
commands defined in the file and how many times each is defined.

NOTE: Each time you define a command using \\<bookmark-bmenu-mode-map>`\\[bmkp-bmenu-define-command]', `\\[bmkp-bmenu-define-full-snapshot-command]', `\\[bmkp-bmenu-define-jump-marked-command], or `\\[bmkp-define-tags-sort-command]',
it is saved in the file.  The new definition is simply appended to the
file - old definitions of the same command are not overwritten.  So
you might want to clean up the file occasionally, to remove any old,
unused definitions.  This is especially advisable if you used `\\[bmkp-bmenu-define-full-snapshot-command]',
because such command definitions can be very large.")

(custom-autoload 'bmkp-bmenu-commands-file "bookmark+-bmu" t)

(defvar bmkp-bmenu-state-file (convert-standard-filename "~/.emacs-bmk-bmenu-state.el") "\
*File for saving `*Bookmark List*' state when you quit bookmark list.
This must be an absolute file name (possibly containing `~') or nil
\(it is not expanded).

The state is also saved when you quit Emacs, even if you don't quit
the bookmark list first (using \\<bookmark-bmenu-mode-map>`\\[bmkp-bmenu-quit]').

Set this to nil if you do not want to restore the bookmark list as it
was the last time you used it.")

(custom-autoload 'bmkp-bmenu-state-file "bookmark+-bmu" t)

(autoload 'bookmark-bmenu-mark "bookmark+-bmu" "\
Mark the bookmark on this line, using mark `>'.

\(fn)" t nil)

(autoload 'bookmark-bmenu-unmark "bookmark+-bmu" "\
Unmark the bookmark on this line, then move down to the next.
Optional BACKUP means move up instead.

\(fn &optional BACKUP)" t nil)

(autoload 'bookmark-bmenu-delete "bookmark+-bmu" "\
Flag this bookmark for deletion, using mark `D'.
Use `\\<bookmark-bmenu-mode-map>\\[bookmark-bmenu-execute-deletions]' to carry out the deletions.

\(fn)" t nil)

(defalias 'list-bookmarks 'bookmark-bmenu-list)

(autoload 'bookmark-bmenu-list "bookmark+-bmu" "\
Display a list of existing bookmarks, in buffer `*Bookmark List*'.
The leftmost column of a bookmark entry shows `D' if the bookmark is
 flagged for deletion, or `>' if it is marked normally.
The second column shows `t' if the bookmark has tags.
The third  column shows `a' if the bookmark has an annotation.

The following faces are used for the list entries.
Use `customize-face' if you want to change the appearance.

 `bmkp-bad-bookmark', `bmkp-bookmark-list', `bmkp-buffer',
 `bmkp-desktop', `bmkp-function', `bmkp-gnus', `bmkp-info',
 `bmkp-local-directory', `bmkp-local-file-without-region',
 `bmkp-local-file-with-region', `bmkp-man', `bmkp-non-file',
 `bmkp-remote-file', `bmkp-sequence', `bmkp-su-or-sudo', `bmkp-url',
 `bmkp-variable-list'.

If option `bmkp-bmenu-state-file' is non-nil then the state of the
displayed bookmark-list is saved when you quit it, and it is restored
when you next use this command.  That saved state is not restored,
however, if it represents a different file from the current bookmark
file.

If you call this interactively when buffer `*Bookmark List*' exists,
that buffer is refreshed to show all current bookmarks, and any
markings are removed.  If you instead want to show the buffer in its
latest state then just do that: use `C-x b' or similar.  If you want
to refresh the displayed buffer, to show the latest state, reflecting
any additions, deletions, renamings, and so on, use \\<bookmark-bmenu-mode-map>`\\[bmkp-bmenu-refresh-menu-list]'.

In Lisp code, non-nil optional argument FILTEREDP means the bookmark
list has been filtered, which means:
 * Use `bmkp-bmenu-title' not the default menu-list title.
 * Do not reset `bmkp-latest-bookmark-alist' to `bookmark-alist'.

\(fn &optional FILTEREDP)" t nil)

(autoload 'bookmark-bmenu-1-window "bookmark+-bmu" "\
Select this line's bookmark, alone, in full frame.
See `bookmark-jump' for info about the prefix arg.

\(fn &optional USE-REGION-P)" t nil)

(autoload 'bookmark-bmenu-2-window "bookmark+-bmu" "\
Select this line's bookmark, with previous buffer in second window.
See `bookmark-jump' for info about the prefix arg.

\(fn &optional USE-REGION-P)" t nil)

(autoload 'bookmark-bmenu-this-window "bookmark+-bmu" "\
Select this line's bookmark in this window.
See `bookmark-jump' for info about the prefix arg.

\(fn &optional USE-REGION-P)" t nil)

(autoload 'bookmark-bmenu-other-window "bookmark+-bmu" "\
Select this line's bookmark in other window.  Show `*Bookmark List*' still.
See `bookmark-jump' for info about the prefix arg.

\(fn &optional USE-REGION-P)" t nil)

(autoload 'bookmark-bmenu-switch-other-window "bookmark+-bmu" "\
Make the other window select this line's bookmark.
The current window remains selected.
See `bookmark-jump' for info about the prefix arg.

\(fn &optional USE-REGION-P)" t nil)

(autoload 'bookmark-bmenu-other-window-with-mouse "bookmark+-bmu" "\
Select clicked bookmark in other window.  Show `*Bookmark List*' still.

\(fn EVENT &optional USE-REGION-P)" t nil)

(autoload 'bookmark-bmenu-show-annotation "bookmark+-bmu" "\
Show the annotation for the current bookmark in another window.

\(fn MSGP)" t nil)

(autoload 'bookmark-bmenu-execute-deletions "bookmark+-bmu" "\
Delete (visible) bookmarks flagged `D'.
With a prefix argument, delete the bookmarks marked `>' instead, after
confirmation.

\(fn &optional MARKEDP)" t nil)

(autoload 'bookmark-bmenu-rename "bookmark+-bmu" "\
Rename bookmark on current line.  Prompts for a new name.

\(fn)" t nil)

(autoload 'bmkp-bmenu-show-only-autonamed "bookmark+-bmu" "\
Display (only) the autonamed bookmarks.

\(fn)" t nil)

(autoload 'bmkp-bmenu-show-only-bookmark-files "bookmark+-bmu" "\
Display (only) the bookmark-file bookmarks.

\(fn)" t nil)

(autoload 'bmkp-bmenu-show-only-desktops "bookmark+-bmu" "\
Display (only) the desktop bookmarks.

\(fn)" t nil)

(autoload 'bmkp-bmenu-show-only-dired "bookmark+-bmu" "\
Display (only) the Dired bookmarks.

\(fn)" t nil)

(autoload 'bmkp-bmenu-show-only-files "bookmark+-bmu" "\
Display a list of file and directory bookmarks (only).
With a prefix argument, do not include remote files or directories.

\(fn ARG)" t nil)

(autoload 'bmkp-bmenu-show-only-non-files "bookmark+-bmu" "\
Display (only) the non-file bookmarks.

\(fn)" t nil)

(autoload 'bmkp-bmenu-show-only-gnus "bookmark+-bmu" "\
Display (only) the Gnus bookmarks.

\(fn)" t nil)

(autoload 'bmkp-bmenu-show-only-info-nodes "bookmark+-bmu" "\
Display (only) the Info bookmarks.

\(fn)" t nil)

(autoload 'bmkp-bmenu-show-only-man-pages "bookmark+-bmu" "\
Display (only) the `man' page bookmarks.

\(fn)" t nil)

(autoload 'bmkp-bmenu-show-only-regions "bookmark+-bmu" "\
Display (only) the bookmarks that record a region.

\(fn)" t nil)

(autoload 'bmkp-bmenu-show-only-variable-lists "bookmark+-bmu" "\
Display (only) the variable-list bookmarks.

\(fn)" t nil)

(autoload 'bmkp-bmenu-show-only-specific-buffer "bookmark+-bmu" "\
Display (only) the bookmarks for BUFFER.
Interactively, read the BUFFER name.
If BUFFER is non-nil, set `bmkp-last-specific-buffer' to it.

\(fn &optional BUFFER)" t nil)

(autoload 'bmkp-bmenu-show-only-specific-file "bookmark+-bmu" "\
Display (only) the bookmarks for FILE, an absolute file name.
Interactively, read the FILE name.
If FILE is non-nil, set `bmkp-last-specific-file' to it.

\(fn &optional FILE)" t nil)

(autoload 'bmkp-bmenu-show-only-urls "bookmark+-bmu" "\
Display (only) the URL bookmarks.

\(fn)" t nil)

(autoload 'bmkp-bmenu-show-only-w3m-urls "bookmark+-bmu" "\
Display (only) the W3M URL bookmarks.

\(fn)" t nil)

(autoload 'bmkp-bmenu-show-all "bookmark+-bmu" "\
Show all bookmarks known to the bookmark list (aka \"menu list\").
Omitted bookmarks are not shown, however.
Also, this does not revert the bookmark list, to bring it up to date.
To revert the list, use `\\<bookmark-bmenu-mode-map>\\[bmkp-bmenu-refresh-menu-list]'.

\(fn)" t nil)

(autoload 'bmkp-bmenu-refresh-menu-list "bookmark+-bmu" "\
Refresh (revert) the bookmark list (\"menu list\").
This brings the displayed list up to date.  It does not change the
current filtering or sorting of the displayed list.

If you want setting a bookmark to refresh the list automatically, you
can use command `bmkp-toggle-bookmark-set-refreshes'.

\(fn)" t nil)

(autoload 'bmkp-bmenu-filter-bookmark-name-incrementally "bookmark+-bmu" "\
Incrementally filter bookmarks by bookmark name using a regexp.

\(fn)" t nil)

(autoload 'bmkp-bmenu-filter-file-name-incrementally "bookmark+-bmu" "\
Incrementally filter bookmarks by file name using a regexp.

\(fn)" t nil)

(autoload 'bmkp-bmenu-filter-annotation-incrementally "bookmark+-bmu" "\
Incrementally filter bookmarks by their annotations using a regexp.

\(fn)" t nil)

(autoload 'bmkp-bmenu-filter-tags-incrementally "bookmark+-bmu" "\
Incrementally filter bookmarks by tags using a regexp.

\(fn)" t nil)

(autoload 'bmkp-bmenu-toggle-show-only-unmarked "bookmark+-bmu" "\
Hide all marked bookmarks.  Repeat to toggle, showing all.

\(fn)" t nil)

(autoload 'bmkp-bmenu-toggle-show-only-marked "bookmark+-bmu" "\
Hide all unmarked bookmarks.  Repeat to toggle, showing all.

\(fn)" t nil)

(autoload 'bmkp-bmenu-mark-all "bookmark+-bmu" "\
Mark all bookmarks, using mark `>'.

\(fn)" t nil)

(autoload 'bmkp-bmenu-unmark-all "bookmark+-bmu" "\
Remove a mark from each bookmark.
Hit the mark character (`>' or `D') to remove those marks,
 or hit `RET' to remove all marks (both `>' and `D').
With a prefix arg, you are queried to unmark each marked bookmark.
Use `\\[help-command]' during querying for help.

\(fn MARK &optional ARG)" t nil)

(autoload 'bmkp-bmenu-regexp-mark "bookmark+-bmu" "\
Mark bookmarks that match REGEXP.
The entire bookmark line is tested: bookmark name and possibly file name.

\(fn REGEXP)" t nil)

(autoload 'bmkp-bmenu-mark-bookmark-file-bookmarks "bookmark+-bmu" "\
Mark bookmark-file bookmarks.

\(fn)" t nil)

(autoload 'bmkp-bmenu-mark-desktop-bookmarks "bookmark+-bmu" "\
Mark desktop bookmarks.

\(fn)" t nil)

(autoload 'bmkp-bmenu-mark-dired-bookmarks "bookmark+-bmu" "\
Mark Dired bookmarks.

\(fn)" t nil)

(autoload 'bmkp-bmenu-mark-file-bookmarks "bookmark+-bmu" "\
Mark file bookmarks.
With a prefix argument, do not mark remote files or directories.

\(fn ARG)" t nil)

(autoload 'bmkp-bmenu-mark-gnus-bookmarks "bookmark+-bmu" "\
Mark Gnus bookmarks.

\(fn)" t nil)

(autoload 'bmkp-bmenu-mark-info-bookmarks "bookmark+-bmu" "\
Mark Info bookmarks.

\(fn)" t nil)

(autoload 'bmkp-bmenu-mark-man-bookmarks "bookmark+-bmu" "\
Mark `man' page bookmarks.

\(fn)" t nil)

(autoload 'bmkp-bmenu-mark-non-file-bookmarks "bookmark+-bmu" "\
Mark non-file bookmarks.

\(fn)" t nil)

(autoload 'bmkp-bmenu-mark-region-bookmarks "bookmark+-bmu" "\
Mark bookmarks that record a region.

\(fn)" t nil)

(autoload 'bmkp-bmenu-mark-specific-buffer-bookmarks "bookmark+-bmu" "\
Mark bookmarks for BUFFER.
Interactively, read the name of the buffer.
If BUFFER is non-nil, set `bmkp-last-specific-buffer' to it.

\(fn &optional BUFFER)" t nil)

(autoload 'bmkp-bmenu-mark-specific-file-bookmarks "bookmark+-bmu" "\
Mark bookmarks for FILE, an absolute file name.
Interactively, read the file name.
If FILE is non-nil, set `bmkp-last-specific-file' to it.

\(fn &optional FILE)" t nil)

(autoload 'bmkp-bmenu-mark-url-bookmarks "bookmark+-bmu" "\
Mark URL bookmarks.

\(fn)" t nil)

(autoload 'bmkp-bmenu-mark-w3m-bookmarks "bookmark+-bmu" "\
Mark W3M (URL) bookmarks.

\(fn)" t nil)

(autoload 'bmkp-bmenu-mark-bookmarks-satisfying "bookmark+-bmu" "\
Mark bookmarks that satisfy predicate PRED.
If you use this interactively, you are responsible for entering a
symbol that names a unnary predicate.  The function you provide is not
checked - it is simply applied to each bookmark to test it.

\(fn PRED)" t nil)

(autoload 'bmkp-bmenu-toggle-marks "bookmark+-bmu" "\
Toggle marks: Unmark all marked bookmarks; mark all unmarked bookmarks.
This affects only the `>' mark, not the `D' flag.

\(fn)" t nil)

(autoload 'bmkp-bmenu-dired-marked "bookmark+-bmu" "\
Dired in another window for the marked file and directory bookmarks.

Absolute file names are used for the entries in the Dired buffer.
The only entries are for the marked files and directories.  These can
be located anywhere.  (In Emacs versions prior to release 23.2, remote
bookmarks are ignored, because of Emacs bug #5478.)

You are prompted for the Dired buffer name.  The `default-directory'
of the buffer is the same as that of buffer `*Bookmark List*'.

\(fn DIRBUFNAME)" t nil)

(autoload 'bmkp-bmenu-delete-marked "bookmark+-bmu" "\
Delete all (visible) bookmarks that are marked `>', after confirmation.

\(fn)" t nil)

(autoload 'bmkp-bmenu-make-sequence-from-marked "bookmark+-bmu" "\
Create or update a sequence bookmark from the visible marked bookmarks.
The bookmarks that are currently marked are recorded as a sequence, in
their current order in buffer `*Bookmark List*'.
When you \"jump\" to the sequence bookmark, the bookmarks in the
sequence are processed in order.

By default, omit the marked bookmarks, after creating the sequence.
With a prefix arg, do not omit them.

If a bookmark with the specified name already exists, it is
overwritten.  If a sequence bookmark with the name already exists,
then you are prompted whether to add the marked bookmarks to the
beginning of the existing sequence (or simply replace it).

Note that another existing sequence bookmark can be marked, and thus
included in the sequence bookmark created or updated.  That is, you
can include other sequences within a sequence bookmark.

Returns the bookmark (internal record) created or updated.

\(fn BOOKMARK-NAME &optional DONT-OMIT-P)" t nil)

(autoload 'bmkp-bmenu-omit "bookmark+-bmu" "\
Omit this bookmark.

\(fn)" t nil)

(autoload 'bmkp-bmenu-omit/unomit-marked "bookmark+-bmu" "\
Omit all marked bookmarks or, if showing only omitted ones, unomit.

\(fn)" t nil)

(autoload 'bmkp-bmenu-omit-marked "bookmark+-bmu" "\
Omit all marked bookmarks.
They will henceforth be invisible to the bookmark list.
You can, however, use \\<bookmark-bmenu-mode-map>`\\[bmkp-bmenu-show-only-omitted]' to see them.
You can then mark some of them and use `\\[bmkp-bmenu-omit/unomit-marked]' to make those marked
 available again for the bookmark list.

\(fn)" t nil)

(autoload 'bmkp-bmenu-unomit-marked "bookmark+-bmu" "\
Remove all marked bookmarks from the list of omitted bookmarks.
They will henceforth be available for display in the bookmark list.
\(In order to see and then mark omitted bookmarks you must use \\<bookmark-bmenu-mode-map>`\\[bmkp-bmenu-show-only-omitted]'.)

\(fn)" t nil)

(autoload 'bmkp-bmenu-show-only-omitted "bookmark+-bmu" "\
Show only the omitted bookmarks.
You can then mark some of them and use `bmkp-bmenu-unomit-marked' to
 make those that are marked available again for the bookmark list.

\(fn)" t nil)

(autoload 'bmkp-bmenu-search-marked-bookmarks-regexp "bookmark+-bmu" "\
Search the marked file bookmarks, in their current order, for REGEXP.
Use `\\[tags-loop-continue]' to advance among the search hits.
Marked directory and non-file bookmarks are ignored.

\(fn REGEXP)" t nil)

(autoload 'bmkp-bmenu-query-replace-marked-bookmarks-regexp "bookmark+-bmu" "\
`query-replace-regexp' FROM with TO, for all marked file bookmarks.
DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (`\\[keyboard-quit]', `RET' or `q'), you can use `\\[tags-loop-continue]' to resume where
you left off.

\(fn FROM TO &optional DELIMITED)" t nil)

(autoload 'bmkp-bmenu-show-only-tagged "bookmark+-bmu" "\
Display (only) the bookmarks that have tags.

\(fn)" t nil)

(autoload 'bmkp-bmenu-remove-all-tags "bookmark+-bmu" "\
Remove all tags from this bookmark.
Interactively, you are required to confirm.

\(fn &optional MUST-CONFIRM-P)" t nil)

(autoload 'bmkp-bmenu-add-tags "bookmark+-bmu" "\
Add some tags to this bookmark.

\(fn)" t nil)

(autoload 'bmkp-bmenu-set-tag-value "bookmark+-bmu" "\
Set the value of one of this bookmark's tags.

\(fn)" t nil)

(autoload 'bmkp-bmenu-set-tag-value-for-marked "bookmark+-bmu" "\
Set the value of TAG to VALUE, for each of the marked bookmarks.
If any of the bookmarks has no tag named TAG, then add one with VALUE.

\(fn TAG VALUE &optional MSGP)" t nil)

(autoload 'bmkp-bmenu-remove-tags "bookmark+-bmu" "\
Remove some tags from this bookmark.

\(fn &optional MSGP)" t nil)

(autoload 'bmkp-bmenu-add-tags-to-marked "bookmark+-bmu" "\
Add TAGS to each of the marked bookmarks.
TAGS is a list of strings.
Hit `RET' to enter each tag, then hit `RET' again after the last tag.
You can use completion to enter each tag, but you are not limited to
choosing existing tags.

\(fn TAGS &optional MSGP)" t nil)

(autoload 'bmkp-bmenu-remove-tags-from-marked "bookmark+-bmu" "\
Remove TAGS from each of the marked bookmarks.
Hit `RET' to enter each tag, then hit `RET' again after the last tag.
You can use completion to enter each tag.

\(fn TAGS &optional MSGP)" t nil)

(autoload 'bmkp-bmenu-mark-bookmarks-tagged-regexp "bookmark+-bmu" "\
Mark bookmarks any of whose tags match REGEXP.
With a prefix arg, mark all that are tagged but with no tags that match.

\(fn REGEXP &optional NOTP)" t nil)

(autoload 'bmkp-bmenu-mark-bookmarks-tagged-all "bookmark+-bmu" "\
Mark all visible bookmarks that are tagged with *each* tag in TAGS.
As a special case, if TAGS is empty, then mark the bookmarks that have
any tags at all (i.e., at least one tag).

With a prefix arg, mark all that are *not* tagged with *any* TAGS.

\(fn TAGS &optional NONEP MSGP)" t nil)

(autoload 'bmkp-bmenu-mark-bookmarks-tagged-none "bookmark+-bmu" "\
Mark all visible bookmarks that are not tagged with *any* tag in TAGS.
As a special case, if TAGS is empty, then mark the bookmarks that have
no tags at all.

With a prefix arg, mark all that are tagged with *each* tag in TAGS.

\(fn TAGS &optional ALLP MSGP)" t nil)

(autoload 'bmkp-bmenu-mark-bookmarks-tagged-some "bookmark+-bmu" "\
Mark all visible bookmarks that are tagged with *some* tag in TAGS.
As a special case, if TAGS is empty, then mark the bookmarks that have
any tags at all.

With a prefix arg, mark all that are *not* tagged with *all* TAGS.

Hit `RET' to enter each tag, then hit `RET' again after the last tag.
You can use completion to enter each tag.

\(fn TAGS &optional SOMENOTP MSGP)" t nil)

(autoload 'bmkp-bmenu-mark-bookmarks-tagged-not-all "bookmark+-bmu" "\
Mark all visible bookmarks that are *not* tagged with *all* TAGS.
As a special case, if TAGS is empty, then mark the bookmarks that have
no tags at all.

With a prefix arg, mark all that are tagged with *some* tag in TAGS.

\(fn TAGS &optional SOMEP MSGP)" t nil)

(autoload 'bmkp-bmenu-unmark-bookmarks-tagged-all "bookmark+-bmu" "\
Unmark all visible bookmarks that are tagged with *each* tag in TAGS.
As a special case, if TAGS is empty, then mark the bookmarks that have
any tags at all.

With a prefix arg, unmark all that are *not* tagged with *any* TAGS.

\(fn TAGS &optional NONEP MSGP)" t nil)

(autoload 'bmkp-bmenu-unmark-bookmarks-tagged-none "bookmark+-bmu" "\
Unmark all visible bookmarks that are *not* tagged with *any* TAGS.
With a prefix arg, unmark all that are tagged with *each* tag in TAGS.
As a special case, if TAGS is empty, then mark the bookmarks that have
no tags at all.

\(fn TAGS &optional ALLP MSGP)" t nil)

(autoload 'bmkp-bmenu-unmark-bookmarks-tagged-some "bookmark+-bmu" "\
Unmark all visible bookmarks that are tagged with *some* tag in TAGS.
As a special case, if TAGS is empty, then unmark the bookmarks that have
any tags at all.

With a prefix arg, unmark all that are *not* tagged with *all* TAGS.

\(fn TAGS &optional SOMENOTP MSGP)" t nil)

(autoload 'bmkp-bmenu-unmark-bookmarks-tagged-not-all "bookmark+-bmu" "\
Unmark all visible bookmarks that are tagged with *some* tag in TAGS.
As a special case, if TAGS is empty, then mark the bookmarks that have
no tags at all.
With a prefix arg, unmark all that are *not* tagged with *all* TAGS.

\(fn TAGS &optional SOMEP MSGP)" t nil)

(autoload 'bmkp-bmenu-w32-open "bookmark+-bmu" "\
Use `w32-browser' to open this bookmark.

\(fn)" t nil)

(autoload 'bmkp-bmenu-w32-open-with-mouse "bookmark+-bmu" "\
Use `w32-browser' to open the bookmark clicked.

\(fn EVENT)" t nil)

(autoload 'bmkp-bmenu-w32-open-select "bookmark+-bmu" "\
Use `w32-browser' to open this bookmark and all marked bookmarks.

\(fn)" t nil)

(autoload 'bmkp-bmenu-mode-status-help "bookmark+-bmu" "\
`describe-mode' + current status of `*Bookmark List*' + face legend.

\(fn)" t nil)

(autoload 'bmkp-bmenu-define-jump-marked-command "bookmark+-bmu" "\
Define a command to jump to a bookmark that is one of those now marked.
The bookmarks marked now will be those that are completion candidates
for the command (but omitted bookmarks are excluded).
Save the command definition in `bmkp-bmenu-commands-file'.

\(fn)" t nil)

(autoload 'bmkp-bmenu-define-command "bookmark+-bmu" "\
Define a command to use the current sort order, filter, and omit list.
Prompt for the command name.  Save the command definition in
`bmkp-bmenu-commands-file'.

The current sort order, filter function, omit list, and title for
buffer `*Bookmark List*' are encapsulated as part of the command.
Use the command at any time to restore them.

\(fn)" t nil)

(autoload 'bmkp-bmenu-define-full-snapshot-command "bookmark+-bmu" "\
Define a command to restore the current bookmark-list state.
Prompt for the command name.  Save the command definition in
`bmkp-bmenu-commands-file'.

Be aware that the command definition can be quite large, since it
copies the current bookmark list and accessory lists (hidden
bookmarks, marked bookmarks, etc.).  For a lighter weight command, use
`bmkp-bmenu-define-full-snapshot-command' instead.  That records only
the omit list and the sort & filter information.

\(fn)" t nil)

(autoload 'bmkp-define-tags-sort-command "bookmark+-bmu" "\
Define a command to sort bookmarks in the bookmark list by tags.
Hit `RET' to enter each tag, then hit `RET' again after the last tag.

The new command sorts first by the first tag in TAGS, then by the
second, and so on.

Besides sorting for these specific tags, any bookmark that has a tag
sorts before one that has no tags.  Otherwise, sorting is by bookmark
name, alphabetically.

The name of the new command is `bmkp-bmenu-sort-' followed by the
specified tags, in order, separated by hyphens (`-').  E.g., for TAGS
\(\"alpha\" \"beta\"), the name is `bmkp-bmenu-sort-alpha-beta'.

\(fn TAGS &optional MSGP)" t nil)

(autoload 'bmkp-bmenu-edit-bookmark "bookmark+-bmu" "\
Edit the bookmark under the cursor: its name and file name.
With a prefix argument, edit the complete bookmark record (the
internal, Lisp form).

\(fn &optional INTERNALP)" t nil)

(autoload 'bmkp-bmenu-quit "bookmark+-bmu" "\
Quit the bookmark list (aka \"menu list\").
If `bmkp-bmenu-state-file' is non-nil, then save the state, to be
restored the next time the bookmark list is shown.  Otherwise, reset
the internal lists that record menu-list markings.

\(fn)" t nil)

(autoload 'bmkp-bmenu-change-sort-order-repeat "bookmark+-bmu" "\
Cycle to the next sort order.
With a prefix arg, reverse current sort order.
This is a repeatable version of `bmkp-bmenu-change-sort-order'.

\(fn ARG)" t nil)

(autoload 'bmkp-bmenu-change-sort-order "bookmark+-bmu" "\
Cycle to the next sort order.
With a prefix arg, reverse the current sort order.

\(fn &optional ARG)" t nil)

(autoload 'bmkp-reverse-sort-order "bookmark+-bmu" "\
Reverse the current bookmark sort order.
If you combine this with \\<bookmark-bmenu-mode-map>`\\[bmkp-reverse-multi-sort-order]', then see the doc for that command.

\(fn)" t nil)

(autoload 'bmkp-reverse-multi-sort-order "bookmark+-bmu" "\
Reverse the application of multi-sorting predicates.
These are the PRED predicates described for option
`bmkp-sort-comparer'.

This reverses the order in which the predicates are tried, and it
also complements the truth value returned by each predicate.

For example, if the list of multi-sorting predicates is (p1 p2 p3),
then the predicates are tried in the order: p3, p2, p1.  And if a
predicate returns true, `(t)', then the effect is as if it returned
false, `(nil)', and vice versa.

The use of multi-sorting predicates tends to group bookmarks, with the
first predicate corresponding to the first bookmark group etc.

The effect of \\<bookmark-bmenu-mode-map>`\\[bmkp-reverse-multi-sort-order]' is roughly as follows:

 - without also `\\[bmkp-reverse-sort-order]', it reverses the bookmark order in each group

 - combined with `\\[bmkp-reverse-sort-order]', it reverses the order of the bookmark
   groups, but not the bookmarks within a group

This is a rough description.  The actual behavior can be complex,
because of how each predicate is defined.  If this description helps
you, fine.  If not, just experiment and see what happens. ;-)

Remember that ordinary `\\[bmkp-reverse-sort-order]' reversal on its own is straightforward.
If you find `\\[bmkp-reverse-multi-sort-order]' confusing or not helpful, then do not use it.

\(fn)" t nil)

(autoload 'bmkp-bmenu-describe-this+move-down "bookmark+-bmu" "\
Describe bookmark of current line, then move down to the next bookmark.
With a prefix argument, show the internal definition of the bookmark.

\(fn &optional DEFN)" t nil)

(autoload 'bmkp-bmenu-describe-this+move-up "bookmark+-bmu" "\
Describe bookmark of current line, then move down to the next bookmark.
With a prefix argument, show the internal definition of the bookmark.

\(fn &optional DEFN)" t nil)

(autoload 'bmkp-bmenu-describe-this-bookmark "bookmark+-bmu" "\
Describe bookmark of current line.
With a prefix argument, show the internal definition of the bookmark.

\(fn &optional DEFN)" t nil)

(autoload 'bmkp-bmenu-describe-marked "bookmark+-bmu" "\
Describe the marked bookmarks.
With a prefix argument, show the internal definitions.

\(fn &optional DEFN)" t nil)

(autoload 'bmkp-bmenu-mouse-3-menu "bookmark+-bmu" "\
Pop-up menu on `mouse-3' for a bookmark listed in `*Bookmark List*'.

\(fn EVENT)" t nil)

;;;***

;;;### (autoloads (bmkp-previous-lighted-this-buffer-repeat bmkp-next-lighted-this-buffer-repeat
;;;;;;  bmkp-previous-lighted-this-buffer bmkp-next-lighted-this-buffer
;;;;;;  bmkp-cycle-lighted-this-buffer-other-window bmkp-cycle-lighted-this-buffer
;;;;;;  bmkp-light-non-autonamed-this-buffer bmkp-light-autonamed-this-buffer
;;;;;;  bmkp-light-bookmarks-in-region bmkp-light-this-buffer bmkp-light-navlist-bookmarks
;;;;;;  bmkp-light-bookmarks bmkp-light-bookmark-this-buffer bmkp-light-bookmark
;;;;;;  bmkp-set-lighting-for-this-buffer bmkp-set-lighting-for-buffer
;;;;;;  bmkp-set-lighting-for-bookmark bmkp-unlight-this-buffer bmkp-unlight-non-autonamed-this-buffer
;;;;;;  bmkp-unlight-autonamed-this-buffer bmkp-unlight-bookmarks
;;;;;;  bmkp-unlight-bookmark-this-buffer bmkp-unlight-bookmark-here
;;;;;;  bmkp-unlight-bookmark bmkp-lighted-jump-other-window bmkp-lighted-jump
;;;;;;  bmkp-bookmarks-lighted-at-point bmkp-bmenu-set-lighting-for-marked
;;;;;;  bmkp-bmenu-set-lighting bmkp-bmenu-unlight-marked bmkp-bmenu-unlight
;;;;;;  bmkp-bmenu-light-marked bmkp-bmenu-light bmkp-bmenu-show-only-lighted
;;;;;;  bmkp-light-threshold bmkp-light-style-non-autonamed bmkp-light-style-autonamed
;;;;;;  bmkp-light-priorities bmkp-auto-light-when-set bmkp-auto-light-when-jump
;;;;;;  bmkp-auto-light-relocate-when-jump-flag) "bookmark+-lit"
;;;;;;  "bookmark-plus/bookmark+-lit.el" (19804 63274))
;;; Generated autoloads from bookmark-plus/bookmark+-lit.el

(defvar bmkp-auto-light-relocate-when-jump-flag t "\
*Non-nil means highlight the relocated, instead of the recorded, position.
This has an effect only when the highlighting style for the bookmark
is `point'.")

(custom-autoload 'bmkp-auto-light-relocate-when-jump-flag "bookmark+-lit" t)

(defvar bmkp-auto-light-when-jump nil "\
*Which bookmarks to automatically highlight when jumped to.")

(custom-autoload 'bmkp-auto-light-when-jump "bookmark+-lit" t)

(defvar bmkp-auto-light-when-set nil "\
*Which bookmarks to automatically highlight when set.")

(custom-autoload 'bmkp-auto-light-when-set "bookmark+-lit" t)

(defvar bmkp-light-priorities '((bmkp-autonamed-overlays . 160) (bmkp-non-autonamed-overlays . 150)) "\
*Priorities of bookmark highlighting overlay types.
As an idea, `ediff' uses 100+, `isearch' uses 1001.")

(custom-autoload 'bmkp-light-priorities "bookmark+-lit" t)

(defvar bmkp-light-style-autonamed (if (not (fboundp 'fringe-columns)) 'line 'line+lfringe) "\
*Default highlight style for autonamed bookmarks.")

(custom-autoload 'bmkp-light-style-autonamed "bookmark+-lit" t)

(defvar bmkp-light-style-non-autonamed (if (not (fboundp 'fringe-columns)) 'line 'line+rfringe) "\
*Default highlight style for non-autonamed bookmarks.")

(custom-autoload 'bmkp-light-style-non-autonamed "bookmark+-lit" t)

(defvar bmkp-light-threshold 100000 "\
*Maximum number of bookmarks to highlight.")

(custom-autoload 'bmkp-light-threshold "bookmark+-lit" t)

(autoload 'bmkp-bmenu-show-only-lighted "bookmark+-lit" "\
Display a list of highlighted bookmarks (only).

\(fn)" t nil)

(autoload 'bmkp-bmenu-light "bookmark+-lit" "\
Highlight the location of this line's bookmark.

\(fn)" t nil)

(autoload 'bmkp-bmenu-light-marked "bookmark+-lit" "\
Highlight the marked bookmarks.

\(fn &optional PARG MSGP)" t nil)

(autoload 'bmkp-bmenu-unlight "bookmark+-lit" "\
Highlight the location of this line's bookmark.

\(fn)" t nil)

(autoload 'bmkp-bmenu-unlight-marked "bookmark+-lit" "\
Unhighlight the marked bookmarks.

\(fn &optional PARG MSGP)" t nil)

(autoload 'bmkp-bmenu-set-lighting "bookmark+-lit" "\
Set the `lighting' property for this line's bookmark.
You are prompted for the highlight style, face, and condition (when).

\(fn STYLE FACE WHEN &optional MSGP)" t nil)

(autoload 'bmkp-bmenu-set-lighting-for-marked "bookmark+-lit" "\
Set the `lighting' property for the marked bookmarks.
You are prompted for the highlight style, face, and condition (when).

\(fn STYLE FACE WHEN &optional MSGP)" t nil)

(autoload 'bmkp-bookmarks-lighted-at-point "bookmark+-lit" "\
Return a list of the bookmarks highlighted at point.
With no prefix arg, return the bookmark names.
With a prefix arg, return the full bookmark data.
Interactively, display the info.
Non-interactively, use the bookmarks at POSITION (default: point).

\(fn &optional POSITION FULLP MSGP)" t nil)

(autoload 'bmkp-lighted-jump "bookmark+-lit" "\
Jump to a highlighted bookmark.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-lighted-jump-other-window "bookmark+-lit" "\
Jump to a highlighted bookmark in another window.
See `bmkp-lighted-jump'.

\(fn BOOKMARK-NAME &optional USE-REGION-P)" t nil)

(autoload 'bmkp-unlight-bookmark "bookmark+-lit" "\
Unhighlight BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record.

\(fn BOOKMARK &optional NOERRORP MSGP)" t nil)

(autoload 'bmkp-unlight-bookmark-here "bookmark+-lit" "\
Unhighlight a bookmark at point or the same line (in that order).

\(fn &optional NOERRORP MSGP)" t nil)

(autoload 'bmkp-unlight-bookmark-this-buffer "bookmark+-lit" "\
Unhighlight a BOOKMARK in this buffer.
BOOKMARK is a bookmark name or a bookmark record.
With a prefix arg, choose from all bookmarks, not just those in this
buffer.

\(fn BOOKMARK &optional NOERRORP MSGP)" t nil)

(autoload 'bmkp-unlight-bookmarks "bookmark+-lit" "\
Unhighlight bookmarks.
A prefix argument determines which bookmarks to unhighlight:
 none    - Current buffer, all bookmarks.
 >= 0    - Current buffer, autonamed bookmarks only.
 < 0     - Current buffer, non-autonamed bookmarks only.
 C-u     - All buffers (all bookmarks).

\(fn &optional OVERLAYS-SYMBOLS THIS-BUFFER-P MSGP)" t nil)

(autoload 'bmkp-unlight-autonamed-this-buffer "bookmark+-lit" "\
Unhighlight autonamed bookmarks.
No prefix arg: unhighlight them only in the current buffer.
Prefix arg, unhighlight them everywhere.

\(fn &optional EVERYWHEREP)" t nil)

(autoload 'bmkp-unlight-non-autonamed-this-buffer "bookmark+-lit" "\
Unhighlight non-autonamed bookmarks.
No prefix arg: unhighlight them only in the current buffer.
Prefix arg, unhighlight them everywhere.

\(fn &optional EVERYWHEREP)" t nil)

(autoload 'bmkp-unlight-this-buffer "bookmark+-lit" "\
Unhighlight all bookmarks in the current buffer.

\(fn)" t nil)

(autoload 'bmkp-set-lighting-for-bookmark "bookmark+-lit" "\
Set the `lighting' property for bookmark BOOKMARK-NAME.
You are prompted for the bookmark, highlight style, face, and condition.
With a prefix argument, do not highlight now.

Non-interactively:
STYLE, FACE, and WHEN are as for a bookmark's `lighting' property
 entries, or nil if no such entry.
Non-nil MSGP means display a highlighting progress message.
Non-nil LIGHT-NOW-P means apply the highlighting now.

\(fn BOOKMARK-NAME STYLE FACE WHEN &optional MSGP LIGHT-NOW-P)" t nil)

(autoload 'bmkp-set-lighting-for-buffer "bookmark+-lit" "\
Set the `lighting' property for each of the bookmarks for BUFFER.
You are prompted for the highlight style, face, and condition (when).
With a prefix argument, do not highlight now.

Non-interactively:
STYLE, FACE, and WHEN are as for a bookmark's `lighting' property
 entries, or nil if no such entry.
Non-nil MSGP means display a highlighting progress message.
Non-nil LIGHT-NOW-P means apply the highlighting now.

\(fn BUFFER STYLE FACE WHEN &optional MSGP LIGHT-NOW-P)" t nil)

(autoload 'bmkp-set-lighting-for-this-buffer "bookmark+-lit" "\
Set the `lighting' property for each of the bookmarks for this buffer.
You are prompted for the highlight style, face, and condition (when).
With a prefix argument, do not highlight now.

Non-interactively:
STYLE, FACE, and WHEN are as for a bookmark's `lighting' property
 entries, or nil if no such entry.
Non-nil MSGP means display a highlighting progress message.
Non-nil LIGHT-NOW-P means apply the highlighting now.

\(fn STYLE FACE WHEN &optional MSGP LIGHT-NOW-P)" t nil)

(autoload 'bmkp-light-bookmark "bookmark+-lit" "\
Highlight BOOKMARK.
With a prefix arg you are prompted for the style and/or face to use:
 Plain prefix arg (`C-u'): prompt for both style and face.
 Numeric non-negative arg: prompt for face.
 Numeric negative arg: prompt for style.

Non-interactively:
 BOOKMARK is a bookmark name or a bookmark record.
 STYLE and FACE override the defaults.
 POINT-P non-nil means highlight point rather than the recorded
  bookmark `position.

\(fn BOOKMARK &optional STYLE FACE MSGP POINTP)" t nil)

(autoload 'bmkp-light-bookmark-this-buffer "bookmark+-lit" "\
Highlight a BOOKMARK in the current buffer.
With a prefix arg you are prompted for the style and/or face to use:
 Plain prefix arg (`C-u'): prompt for both style and face.
 Numeric non-negative arg: prompt for face.
 Numeric negative arg: prompt for style.
See `bmkp-light-boookmark' for argument descriptions.

\(fn BOOKMARK &optional STYLE FACE MSGP)" t nil)

(autoload 'bmkp-light-bookmarks "bookmark+-lit" "\
Highlight bookmarks.
A prefix argument determines which bookmarks to highlight:
 none    - Current buffer, all bookmarks.
 = 0     - Current buffer, highlighted bookmarks only (rehighlight).
 > 0     - Current buffer, autonamed bookmarks only.
 < 0     - Current buffer, non-autonamed bookmarks only.
 C-u     - All buffers (all bookmarks) - after confirmation.
 C-u C-u - Navlist (all bookmarks).

Non-interactively, ALIST is the alist of bookmarks to highlight.

\(fn &optional ALIST OVERLAYS-SYMBOLS MSGP)" t nil)

(autoload 'bmkp-light-navlist-bookmarks "bookmark+-lit" "\
Highlight bookmarks in the navigation list.
No prefix arg:   all bookmarks.
Prefix arg >= 0: autonamed bookmarks only.
Prefix arg < 0:  non-autonamed bookmarks only.

\(fn &optional OVERLAYS-SYMBOLS MSGP)" t nil)

(autoload 'bmkp-light-this-buffer "bookmark+-lit" "\
Highlight bookmarks in the current buffer.
No prefix arg:   all bookmarks.
Prefix arg >= 0: autonamed bookmarks only.
Prefix arg < 0:  non-autonamed bookmarks only.

\(fn &optional OVERLAYS-SYMBOLS MSGP)" t nil)

(autoload 'bmkp-light-bookmarks-in-region "bookmark+-lit" "\
Highlight bookmarks in the region.
No prefix arg:   all bookmarks.
Prefix arg >= 0: autonamed bookmarks only.
Prefix arg < 0:  non-autonamed bookmarks only.

\(fn START END &optional OVERLAYS-SYMBOLS MSGP)" t nil)

(autoload 'bmkp-light-autonamed-this-buffer "bookmark+-lit" "\
Highlight all autonamed bookmarks.

\(fn &optional MSGP)" t nil)

(autoload 'bmkp-light-non-autonamed-this-buffer "bookmark+-lit" "\
Highlight all non-autonamed bookmarks.

\(fn &optional MSGP)" t nil)

(autoload 'bmkp-cycle-lighted-this-buffer "bookmark+-lit" "\
Cycle through highlighted bookmarks in this buffer by INCREMENT.
Positive INCREMENT cycles forward.  Negative INCREMENT cycles backward.
Interactively, the prefix arg determines INCREMENT:
 Plain `C-u': 1
 otherwise: the numeric prefix arg value 

To change the sort order, you can filter the `*Bookmark List*' to show
only highlighted bookmarks for this buffer, sort the bookmarks there,
and use `\\[bmkp-choose-navlist-from-bookmark-list]', choosing `CURRENT *Bookmark List*' as the
navigation list.

Then you can cycle the bookmarks using `bookmark-cycle'
\(`\\[bmkp-next-bookmark-repeat]' etc.), instead of `bookmark-cycle-lighted-this-buffer'.

In Lisp code:
 Non-nil OTHER-WINDOW means jump to the bookmark in another window.
 Non-nil STARTOVERP means reset `bmkp-current-nav-bookmark' to the
 first bookmark in the navlist.

\(fn INCREMENT &optional OTHER-WINDOW STARTOVERP)" t nil)

(autoload 'bmkp-cycle-lighted-this-buffer-other-window "bookmark+-lit" "\
Same as `bmkp-cycle-lighted-this-buffer' but uses another window.

\(fn INCREMENT &optional STARTOVERP)" t nil)

(autoload 'bmkp-next-lighted-this-buffer "bookmark+-lit" "\
Jump to the Nth-next highlighted bookmark in the current buffer.
N defaults to 1, meaning the next one.
Plain `C-u' means start over at the first one.
See also `bmkp-cycle-lighted-this-buffer'.

\(fn N &optional STARTOVERP)" t nil)

(autoload 'bmkp-previous-lighted-this-buffer "bookmark+-lit" "\
Jump to the Nth-previous highlighted bookmark in the current buffer.
See `bmkp-next-lighted-this-buffer'.

\(fn N &optional STARTOVERP)" t nil)

(autoload 'bmkp-next-lighted-this-buffer-repeat "bookmark+-lit" "\
Jump to the Nth next highlighted bookmark in the current buffer.
This is a repeatable version of `bmkp-next-bookmark-this-buffer'.
N defaults to 1, meaning the next one.
Plain `C-u' means start over at the first one (and no repeat).

\(fn ARG)" t nil)

(autoload 'bmkp-previous-lighted-this-buffer-repeat "bookmark+-lit" "\
Jump to the Nth previous highlighted bookmark in the current buffer.
See `bmkp-next-lighted-this-buffer-repeat'.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (bmkp-menu-bar-make-toggle bmkp-define-file-sort-predicate
;;;;;;  bmkp-define-sort-command bmkp-define-next+prev-cycle-commands
;;;;;;  bmkp-define-cycle-command) "bookmark+-mac" "bookmark-plus/bookmark+-mac.el"
;;;;;;  (19804 63202))
;;; Generated autoloads from bookmark-plus/bookmark+-mac.el

(autoload 'bmkp-define-cycle-command "bookmark+-mac" "\
Define a cycling command for bookmarks of type TYPE.
Non-nil OTHERP means define a command that cycles in another window.

\(fn TYPE &optional OTHERP)" nil (quote macro))

(autoload 'bmkp-define-next+prev-cycle-commands "bookmark+-mac" "\
Define `next' and `previous' commands for bookmarks of type TYPE.

\(fn TYPE)" nil (quote macro))

(autoload 'bmkp-define-sort-command "bookmark+-mac" "\
Define a command to sort bookmarks in the bookmark list by SORT-ORDER.
SORT-ORDER is a short string or symbol describing the sorting method.
Examples: \"by last access time\", \"by bookmark name\".

The new command is named by replacing any spaces in SORT-ORDER with
hyphens (`-') and then adding the prefix `bmkp-bmenu-sort-'.  Example:
`bmkp-bmenu-sort-by-bookmark-name', for SORT-ORDER `by bookmark name'.

COMPARER compares two bookmarks, returning non-nil if and only if the
first bookmark sorts before the second.  It must be acceptable as a
value of `bmkp-sort-comparer'.  That is, it is either nil, a
predicate, or a list ((PRED...) FINAL-PRED).  See the doc for
`bmkp-sort-comparer'.

DOC-STRING is the doc string of the new command.

\(fn SORT-ORDER COMPARER DOC-STRING)" nil (quote macro))

(autoload 'bmkp-define-file-sort-predicate "bookmark+-mac" "\
Define a predicate for sorting bookmarks by file attribute ATT-NB.
See function `file-attributes' for the meanings of the various file
attribute numbers.

String attribute values sort alphabetically; numerical values sort
numerically; nil sorts before t.

For ATT-NB 0 (file type), a file sorts before a symlink, which sorts
before a directory.

For ATT-NB 2 or 3 (uid, gid), a numerical value sorts before a string
value.

A bookmark that has file attributes sorts before a bookmark that does
not.  A file bookmark sorts before a non-file bookmark.  Only local
files are tested for attributes - remote-file bookmarks are treated
here like non-file bookmarks.

\(fn ATT-NB)" nil (quote macro))

(autoload 'bmkp-menu-bar-make-toggle "bookmark+-mac" "\
Return a valid `menu-bar-make-toggle' call in Emacs 20 or later.
NAME is the name of the toggle command to define.
VARIABLE is the variable to set.
DOC is the menu-item name.
MESSAGE is the toggle message, minus status.
HELP is `:help' string.
BODY is the function body to use.  If present, it is responsible for
setting the variable and displaying a status message (not MESSAGE).

\(fn NAME VARIABLE DOC MESSAGE HELP &rest BODY)" nil (quote macro))

;;;***

;;;### (autoloads (semantic-bovine-debug-create-frame) "bovine-debug"
;;;;;;  "cedet/semantic/bovine/bovine-debug.el" (19114 40244))
;;; Generated autoloads from cedet/semantic/bovine/bovine-debug.el

(autoload 'semantic-bovine-debug-create-frame "bovine-debug" "\
Create one bovine frame.
NONTERM is the name of a rule we are currently parsing.
RULE is the index into the list of rules in NONTERM.
MATCH is the index into the list of matches in RULE.
For example:
  this: that
      | other thing
      | here
      ;
The NONTERM is THIS.
The RULE is for \"thing\" is 1.
The MATCH for \"thing\" is 1.
COLLECTION is a list of `things' that have been matched so far.
LEXTOKEN, is a token returned by the lexer which is being matched.

\(fn NONTERM RULE MATCH COLLECTION LEXTOKEN)" nil nil)

(eieio-defclass-autoload 'semantic-bovine-debug-parser '(semantic-debug-parser) "bovine-debug" "Represents a parser and its state.")

;;;***

;;;### (autoloads (bovine-grammar-mode) "bovine-grammar" "cedet/semantic/bovine/bovine-grammar.el"
;;;;;;  (18791 59610))
;;; Generated autoloads from cedet/semantic/bovine/bovine-grammar.el

(autoload 'bovine-grammar-mode "bovine-grammar" "\
Major mode for editing Bovine grammars.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.by$" . bovine-grammar-mode))

(eval-after-load "speedbar" '(speedbar-add-supported-extension ".by"))

;;;***

;;;### (autoloads (bc-list bc-clear bc-goto-current bc-local-next
;;;;;;  bc-local-previous bc-next bc-previous bc-set) "breadcrumb"
;;;;;;  "breadcrumb.el" (19830 6389))
;;; Generated autoloads from breadcrumb.el

(autoload 'bc-set "breadcrumb" "\
Set a bookmark at the current buffer and current position.

\(fn)" t nil)

(autoload 'bc-previous "breadcrumb" "\
Jump to the previous bookmark.

\(fn)" t nil)

(autoload 'bc-next "breadcrumb" "\
Jump to the next bookmark.

\(fn)" t nil)

(autoload 'bc-local-previous "breadcrumb" "\
Jump to the previous bookmark in the local buffer.

\(fn)" t nil)

(autoload 'bc-local-next "breadcrumb" "\
Jump to the next bookmark in the local buffer.

\(fn)" t nil)

(autoload 'bc-goto-current "breadcrumb" "\
Jump to the current bookmark.

\(fn)" t nil)

(autoload 'bc-clear "breadcrumb" "\
Clear all the breadcrumb bookmarks in the queue.

\(fn)" t nil)

(autoload 'bc-list "breadcrumb" "\
Display the breadcrumb bookmarks in the buffer `*Breadcrumb Bookmarks*' to allow interactive management of them.

\(fn)" t nil)

;;;***

;;;### (autoloads (call-tree) "call-tree" "cedet/eieio/call-tree.el"
;;;;;;  (17213 40289))
;;; Generated autoloads from cedet/eieio/call-tree.el

(autoload 'call-tree "call-tree" "\
Build a call tree to show all functions called by FUNC.

\(fn FUNC)" t nil)

;;;***

;;;### (autoloads (cedet-update-autoloads) "cedet-autogen" "cedet/common/cedet-autogen.el"
;;;;;;  (18540 13339))
;;; Generated autoloads from cedet/common/cedet-autogen.el

(autoload 'cedet-update-autoloads "cedet-autogen" "\
Update autoloads in file LOADDEFS from sources.
Optional argument DIRECTORY, specifies the directory to scan for
autoloads.  It defaults to the current directory.
DIRECTORIES is a list of extra directory to scan.  Those directory
names are relative to DIRECTORY.  If DIRECTORIES is nil try to scan
sub directories of DIRECTORY where a `cedet-autogen-tagfile' file
exists.

\(fn LOADDEFS &optional DIRECTORY &rest DIRECTORIES)" t nil)

;;;***

;;;### (autoloads (cedet-compat-utest) "cedet-compat" "cedet/common/cedet-compat.el"
;;;;;;  (19390 32498))
;;; Generated autoloads from cedet/common/cedet-compat.el

(if (or (featurep 'xemacs) (inversion-test 'emacs "22.0")) (defalias 'cedet-split-string 'cedet-split-string-1) (defalias 'cedet-split-string 'split-string))

(when (not (fboundp 'with-no-warnings)) (put 'with-no-warnings 'lisp-indent-function 0) (defun with-no-warnings (&rest body) "Copied from `with-no-warnings' in Emacs 23.\nLike `progn', but prevents compiler warnings in the body.\nNote: Doesn't work if this version is being loaded." (car (last body))))

(autoload 'cedet-compat-utest "cedet-compat" "\
Test compatability functions.

\(fn)" t nil)

;;;***

;;;### (autoloads (cedet-cscope-version-check cedet-cscope-expand-filename
;;;;;;  cedet-cscope-search cedet-cscope-command) "cedet-cscope"
;;;;;;  "cedet/common/cedet-cscope.el" (19531 32302))
;;; Generated autoloads from cedet/common/cedet-cscope.el

(defvar cedet-cscope-command "cscope" "\
Command name for the CScope executable.")

(custom-autoload 'cedet-cscope-command "cedet-cscope" t)

(autoload 'cedet-cscope-search "cedet-cscope" "\
Perform a search with CScope, return the created buffer.
SEARCHTEXT is text to find.
TEXTTYPE is the type of text, such as 'regexp, 'string, 'tagname,
'tagregexp, or 'tagcompletions.
TYPE is the type of search, meaning that SEARCHTEXT is compared to
filename, tagname (tags table), references (uses of a tag) , or
symbol (uses of something not in the tag table.)
SCOPE is the scope of the search, such as 'project or 'subdirs.

\(fn SEARCHTEXT TEXTTYPE TYPE SCOPE)" nil nil)

(autoload 'cedet-cscope-expand-filename "cedet-cscope" "\
Expand the FILENAME with CScope.
Return a fully qualified filename.

\(fn FILENAME)" t nil)

(autoload 'cedet-cscope-version-check "cedet-cscope" "\
Check the version of the installed CScope command.
If optional programatic argument NOERROR is non-nil, then
instead of throwing an error if cscope isn't available, then
return nil.

\(fn &optional NOERROR)" t nil)

;;;***

;;;### (autoloads nil "cedet-edebug" "cedet/common/cedet-edebug.el"
;;;;;;  (19115 1949))
;;; Generated autoloads from cedet/common/cedet-edebug.el

(add-hook 'edebug-setup-hook (lambda nil (require 'cedet-edebug) (defalias 'edebug-prin1-to-string 'cedet-edebug-prin1-to-string) (define-key edebug-mode-map "A" 'data-debug-edebug-expr)))

(add-hook 'debugger-mode-hook (lambda nil (require 'cedet-edebug) (define-key debugger-mode-map "A" 'data-debug-edebug-expr)))

;;;***

;;;### (autoloads (cedet-files-utest) "cedet-files" "cedet/common/cedet-files.el"
;;;;;;  (19373 12949))
;;; Generated autoloads from cedet/common/cedet-files.el

(autoload 'cedet-files-utest "cedet-files" "\
Test out some file name conversions.

\(fn)" t nil)

;;;***

;;;### (autoloads (cedet-gnu-global-version-check cedet-gnu-global-root
;;;;;;  cedet-gnu-global-show-root cedet-gnu-global-expand-filename
;;;;;;  cedet-gnu-global-search cedet-global-gtags-command cedet-global-command)
;;;;;;  "cedet-global" "cedet/common/cedet-global.el" (19531 32317))
;;; Generated autoloads from cedet/common/cedet-global.el

(defvar cedet-global-command "global" "\
Command name for the GNU Global executable.")

(custom-autoload 'cedet-global-command "cedet-global" t)

(defvar cedet-global-gtags-command "gtags" "\
Command name for the GNU Global gtags executable.
GTAGS is used to create the tags table queried by the 'global' command.")

(custom-autoload 'cedet-global-gtags-command "cedet-global" t)

(autoload 'cedet-gnu-global-search "cedet-global" "\
Perform a search with GNU Global, return the created buffer.
SEARCHTEXT is text to find.
TEXTTYPE is the type of text, such as 'regexp, 'string, 'tagname,
'tagregexp, or 'tagcompletions.
TYPE is the type of search, meaning that SEARCHTEXT is compared to
filename, tagname (tags table), references (uses of a tag) , or
symbol (uses of something not in the tag table.)
SCOPE is the scope of the search, such as 'project or 'subdirs.

\(fn SEARCHTEXT TEXTTYPE TYPE SCOPE)" nil nil)

(autoload 'cedet-gnu-global-expand-filename "cedet-global" "\
Expand the FILENAME with GNU Global.
Return a fully qualified filename.

\(fn FILENAME)" t nil)

(autoload 'cedet-gnu-global-show-root "cedet-global" "\
Show the root of a GNU Global area under the current buffer.

\(fn)" t nil)

(autoload 'cedet-gnu-global-root "cedet-global" "\
Return the root of any GNU Global scanned project.
If a default starting DIR is not specified, the current buffer's
`default-directory' is used.

\(fn &optional DIR)" nil nil)

(autoload 'cedet-gnu-global-version-check "cedet-global" "\
Check the version of the installed GNU Global command.
If optional programatic argument NOERROR is non-nil, then
instead of throwing an error if Global isn't available, then
return nil.

\(fn &optional NOERROR)" t nil)

;;;***

;;;### (autoloads (cedet-graphviz-dot-version-check cedet-graphviz-neato-command
;;;;;;  cedet-graphviz-dot-command) "cedet-graphviz" "cedet/common/cedet-graphviz.el"
;;;;;;  (19390 36297))
;;; Generated autoloads from cedet/common/cedet-graphviz.el

(defvar cedet-graphviz-dot-command "dot" "\
Command name for the Graphviz DOT executable.")

(custom-autoload 'cedet-graphviz-dot-command "cedet-graphviz" t)

(defvar cedet-graphviz-neato-command "neato" "\
Command name for the Graphviz NEATO executable.")

(custom-autoload 'cedet-graphviz-neato-command "cedet-graphviz" t)

(autoload 'cedet-graphviz-dot-version-check "cedet-graphviz" "\
Check the version of the installed Graphviz dot command.
If optional programatic argument NOERROR is non-nil, then
instead of throwing an error if Global isn't available, then
return nil.

\(fn &optional NOERROR)" t nil)

;;;***

;;;### (autoloads (cedet-idutils-version-check cedet-idutils-expand-filename
;;;;;;  cedet-idutils-make-command cedet-idutils-token-command cedet-idutils-file-command)
;;;;;;  "cedet-idutils" "cedet/common/cedet-idutils.el" (19531 32333))
;;; Generated autoloads from cedet/common/cedet-idutils.el

(defvar cedet-idutils-file-command "fnid" "\
Command name for the ID Utils executable for searching file names.")

(custom-autoload 'cedet-idutils-file-command "cedet-idutils" t)

(defvar cedet-idutils-token-command "lid" "\
Command name for the ID Utils executable for searching for tokens.")

(custom-autoload 'cedet-idutils-token-command "cedet-idutils" t)

(defvar cedet-idutils-make-command "mkid" "\
Command name for the ID Utils executable for creating token databases.")

(custom-autoload 'cedet-idutils-make-command "cedet-idutils" t)

(autoload 'cedet-idutils-expand-filename "cedet-idutils" "\
Expand the FILENAME with ID Utils.
Return a filename relative to the default directory.

\(fn FILENAME)" t nil)

(autoload 'cedet-idutils-version-check "cedet-idutils" "\
Check the version of the installed ID Utils command.
If optional programatic argument NOERROR is non-nil, then
instead of throwing an error if Global isn't available, then
return nil.

\(fn &optional NOERROR)" t nil)

;;;***

;;;### (autoloads (global-cedet-m3-minor-mode cedet-m3-minor-mode)
;;;;;;  "cedet-m3" "cedet/common/cedet-m3.el" (19534 9680))
;;; Generated autoloads from cedet/common/cedet-m3.el

(autoload 'cedet-m3-minor-mode "cedet-m3" "\
Toggle cedet-m3 minor mode, a mouse 3 context menu.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\\{cedet-m3-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'global-cedet-m3-minor-mode "cedet-m3" "\
Toggle global use of cedet-m3 minor mode.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (cedet-utest-batch cedet-utest) "cedet-utests"
;;;;;;  "cedet/common/cedet-utests.el" (19408 58973))
;;; Generated autoloads from cedet/common/cedet-utests.el

(autoload 'cedet-utest "cedet-utests" "\
Run the CEDET unittests.
EXIT-ON-ERROR causes the test suite to exit on an error, instead
of just logging the error.

\(fn &optional EXIT-ON-ERROR)" t nil)

(autoload 'cedet-utest-batch "cedet-utests" "\
Run the CEDET unit test in BATCH mode.

\(fn)" nil nil)

;;;***

;;;### (autoloads (chart-test-it-all) "chart" "cedet/eieio/chart.el"
;;;;;;  (19336 38169))
;;; Generated autoloads from cedet/eieio/chart.el

(autoload 'chart-test-it-all "chart" "\
Test out various charting features.

\(fn)" t nil)

;;;***

;;;### (autoloads (cheat) "cheat" "cheat.el" (19821 116))
;;; Generated autoloads from cheat.el

(autoload 'cheat "cheat" "\
Show the specified cheat sheet.

If SILENT is non-nil then do not print any output, but return it
as a string instead.

\(fn NAME &optional SILENT)" t nil)

;;;***

;;;### (autoloads (circe) "circe" "circe/circe.el" (18727 63829))
;;; Generated autoloads from circe/circe.el

(autoload 'circe "circe" "\
Connect to the IRC server HOST at SERVICE.
NETWORK is the shorthand used for indicating where we're connected
to. (defaults to HOST)
PASS is the password.
NICK is the nick name to use (defaults to `circe-default-nick')
USER is the user name to use (defaults to `circe-default-user')
REALNAME is the real name to use (defaults to `circe-default-realname')

\(fn HOST SERVICE &optional NETWORK PASS NICK USER REALNAME)" t nil)

;;;***

;;;### (autoloads (enable-circe-highlight-all-nicks) "circe-highlight-all-nicks"
;;;;;;  "circe/circe-highlight-all-nicks.el" (17197 53002))
;;; Generated autoloads from circe/circe-highlight-all-nicks.el

(autoload 'enable-circe-highlight-all-nicks "circe-highlight-all-nicks" "\
Enable the Highlight Nicks module for Circe.
This module highlights all occurances of nicks in the current
channel in messages of other people.

\(fn)" t nil)

;;;***

;;;### (autoloads (enable-circe-log) "circe-log" "circe/circe-log.el"
;;;;;;  (18080 62587))
;;; Generated autoloads from circe/circe-log.el

(autoload 'enable-circe-log "circe-log" "\
Enables automatic logging for all buffers matching
`circe-log-buffer-regexp' and not matching
`circe-log-exlude-buffer-regexp'.

\(fn)" t nil)

;;;***

;;;### (autoloads (turn-on-cldoc-mode cldoc-mode cldoc-minor-mode-string
;;;;;;  cldoc-mode) "cldoc" "cldoc.el" (18227 38128))
;;; Generated autoloads from cldoc.el

(defvar cldoc-mode nil "\
*If non-nil, show the defined parameters for the elisp function near point.

For the emacs lisp function at the beginning of the sexp which point is
within, show the defined parameters for the function in the echo area.
This information is extracted directly from the function or macro if it is
in pure lisp.  If the emacs function is a subr, the parameters are obtained
from the documentation string if possible.

If point is over a documented variable, print that variable's docstring
instead.

This variable is buffer-local.")

(custom-autoload 'cldoc-mode "cldoc" t)

(defvar cldoc-minor-mode-string " Cldoc" "\
*String to display in mode line when Cldoc Mode is enabled.")

(custom-autoload 'cldoc-minor-mode-string "cldoc" t)

(cond ((fboundp 'add-minor-mode) (add-minor-mode 'cldoc-mode 'cldoc-minor-mode-string)) ((assq 'cldoc-mode (default-value 'minor-mode-alist))) (t (setq-default minor-mode-alist (append (default-value 'minor-mode-alist) '((cldoc-mode cldoc-minor-mode-string))))))

(autoload 'cldoc-mode "cldoc" "\
*Enable or disable cldoc mode.
See documentation for the variable of the same name for more details.

If called interactively with no prefix argument, toggle current condition
of the mode.
If called with a positive or negative prefix argument, enable or disable
the mode, respectively.

\(fn &optional PREFIX)" t nil)

(autoload 'turn-on-cldoc-mode "cldoc" "\
Unequivocally turn on cldoc-mode (see variable documentation).

\(fn)" t nil)

;;;***

;;;### (autoloads (cogre) "cogre" "cedet/cogre/cogre.el" (19403 31744))
;;; Generated autoloads from cedet/cogre/cogre.el

(eieio-defclass-autoload 'cogre-base-graph '(eieio-persistent) "cogre" "A Connected Graph.\na connected graph contains a series of nodes and links which are\nrendered in a buffer, or serialized to disk.")

(eieio-defclass-autoload 'cogre-graph-element '(eieio-named) "cogre" "A Graph Element.\nGraph elements are anything that is drawn into a `cogre-base-graph'.\nGraph elements have a method for marking themselves dirty.")

(eieio-defclass-autoload 'cogre-node '(cogre-graph-element) "cogre" "Connected Graph node.\nNodes are regions with a fill color, and some amount of text representing\na status, or values.")

(eieio-defclass-autoload 'cogre-link '(cogre-graph-element) "cogre" "Connected Graph link.\nLinks are lines drawn between two nodes, or possibly loose in space\nas an intermediate step.  Some links have text describing what they\ndo, and most links have special markers on one end or another, such as\narrows or circles.")

(eieio-defclass-autoload 'cogre-arrow '(cogre-link) "cogre" "This type of link is a simple arrow.")

(autoload 'cogre "cogre" "\
Create a new graph not associated with a buffer.
The new graph will be given NAME.  See `cogre-mode' for details.
Optional argument GRAPH-CLASS indicates the type of graph to create.

\(fn NAME &optional GRAPH-CLASS)" t nil)

;;;***

;;;### (autoloads (cogre-export-ascii) "cogre-ascii" "cedet/cogre/cogre-ascii.el"
;;;;;;  (18912 13351))
;;; Generated autoloads from cedet/cogre/cogre-ascii.el

(autoload 'cogre-export-ascii "cogre-ascii" "\
Export the current diagram into an ASCII buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (cogre-export-utest cogre-export-dot-method cogre-export-dot-postscript-print
;;;;;;  cogre-export-dot-png cogre-export-dot) "cogre-convert" "cedet/cogre/cogre-convert.el"
;;;;;;  (19407 15770))
;;; Generated autoloads from cedet/cogre/cogre-convert.el

(autoload 'cogre-export-dot "cogre-convert" "\
Export the current COGRE graph to DOT notation.
DOT is a part of GraphViz.

\(fn)" t nil)

(autoload 'cogre-export-dot-png "cogre-convert" "\
Export the current COGRE graph to DOT, then convert that to PNG.
The png file is then displayed in an Emacs buffer.
DOT is a part of GraphVis.

\(fn)" t nil)

(autoload 'cogre-export-dot-postscript-print "cogre-convert" "\
Print the current graph.
This is done by exporting the current COGRE graph to DOT, then
convert that to Postscript before printing.
DOT is a part of GraphVis.

\(fn)" t nil)

(autoload 'cogre-export-dot-method "cogre-convert" "\
Convert G into DOT syntax of semantic tags.

\(fn (G cogre-base-graph))" nil nil)

(autoload 'cogre-export-utest "cogre-convert" "\
Run all the COGRE structured export/convert test.

\(fn)" t nil)

;;;***

;;;### (autoloads (cogre-dot-mode) "cogre-dot-mode" "cedet/cogre/cogre-dot-mode.el"
;;;;;;  (18906 40825))
;;; Generated autoloads from cedet/cogre/cogre-dot-mode.el

(autoload 'cogre-dot-mode "cogre-dot-mode" "\
Major mode for the dot language.
This is a mini-mode that will first attempt to load and install
`graphviz-dot-mode' in this buffer.  If that fails, it installs
the syntax table, and runs a hook needed to get Semantic working
as a parsing engine.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.dot\\'" . cogre-dot-mode))

;;;***

;;;### (autoloads (cogre-layout) "cogre-layout" "cedet/cogre/cogre-layout.el"
;;;;;;  (19372 4887))
;;; Generated autoloads from cedet/cogre/cogre-layout.el

(autoload 'cogre-layout "cogre-layout" "\
Layout the current graph.
This function depends on graphviz `dot' program.

\(fn)" t nil)

;;;***

;;;### (autoloads (cogre-mode) "cogre-mode" "cedet/cogre/cogre-mode.el"
;;;;;;  (19390 34820))
;;; Generated autoloads from cedet/cogre/cogre-mode.el

(autoload 'cogre-mode "cogre-mode" "\
Connected Graph Editor Mode.
\\{cogre-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist (cons "\\.cgr\\'" 'cogre-mode))

;;;***

;;;### (autoloads (cogre-periodic-utest cogre-periodic) "cogre-periodic"
;;;;;;  "cedet/cogre/cogre-periodic.el" (18906 45093))
;;; Generated autoloads from cedet/cogre/cogre-periodic.el

(autoload 'cogre-periodic "cogre-periodic" "\
Create a periodic table of COGRE objects.

\(fn)" t nil)

(autoload 'cogre-periodic-utest "cogre-periodic" "\
Run the cogre periodic table for unit testing.
Also test various output mechanisms from the periodic table.

\(fn)" t nil)

;;;***

;;;### (autoloads (cogre-uml-quick-class cogre-export-code cogre-semantic-tag-to-node)
;;;;;;  "cogre-semantic" "cedet/cogre/cogre-semantic.el" (19390 34606))
;;; Generated autoloads from cedet/cogre/cogre-semantic.el

(autoload 'cogre-semantic-tag-to-node "cogre-semantic" "\
Convert the Semantic tag TAG into a COGRE node.
Only handles data types nodes.
To convert function/variables into methods or attributes in
an existing COGRE node, see @TODO - do that.

\(fn TAG)" nil nil)

(autoload 'cogre-export-code "cogre-semantic" "\
Export the current graph into source-code in FILE.
Uses `cogre-export-semantic' to convert into Semantic tags.
Uses `cogre-srecode-setup' to setup SRecode for code generation.

\(fn FILE)" t nil)

(autoload 'cogre-uml-quick-class "cogre-semantic" "\
Create a new UML diagram based on CLASS showing only immediate lineage.
The parent to CLASS, CLASS, and all of CLASSes children will be shown.

\(fn CLASS)" t nil)

;;;***

;;;### (autoloads (srecode-semantic-handle-:dot srecode-semantic-handle-:cogre
;;;;;;  cogre-srecode-setup) "cogre-srecode" "cedet/cogre/cogre-srecode.el"
;;;;;;  (19345 48423))
;;; Generated autoloads from cedet/cogre/cogre-srecode.el

(autoload 'cogre-srecode-setup "cogre-srecode" "\
Update various paths to get SRecode to identify COGRE macros.

\(fn)" nil nil)

(autoload 'srecode-semantic-handle-:cogre "cogre-srecode" "\
Add macros to dictionary DICT based on COGRE data.

\(fn DICT)" nil nil)

(eval-after-load "srecode-map" '(cogre-srecode-setup))

(autoload 'srecode-semantic-handle-:dot "cogre-srecode" "\
Add macros to dictionary DICT based on the current DOT buffer.

\(fn DICT)" nil nil)

;;;***

;;;### (autoloads (cogre-uml-sort-for-lineage cogre-uml-enable-unicode)
;;;;;;  "cogre-uml" "cedet/cogre/cogre-uml.el" (19390 34401))
;;; Generated autoloads from cedet/cogre/cogre-uml.el

(eieio-defclass-autoload 'cogre-package '(cogre-node) "cogre-uml" "A Package node.\nPackages represent other class diagrams, and list the major nodes\nwithin them.  They can be linked by dependency links.")

(eieio-defclass-autoload 'cogre-note '(cogre-node) "cogre-uml" "An note node.\nNotes are used to add annotations inside a graph.\nNotes are generally linked to some node, and are supposed to look\nlike a little pieces of paper.")

(eieio-defclass-autoload 'cogre-scoped-node '(cogre-node) "cogre-uml" "A UML node that has a package specifier within which it is scoped.")

(eieio-defclass-autoload 'cogre-class '(cogre-scoped-node) "cogre-uml" "A Class node.\nClass nodes represent a class, and can list the attributes and methods\nwithin them.  Classes can have attribute links, and class hierarchy links.")

(eieio-defclass-autoload 'cogre-instance '(cogre-scoped-node) "cogre-uml" "An instance node.\nInstances are used in instance diagrams.\nInstances are linked together with plain links.")

(eieio-defclass-autoload 'cogre-inherit '(cogre-link) "cogre-uml" "This type of link indicates that the two nodes reference infer inheritance.\nThe `start' node is the child, and the `end' node is the parent.\nThis is supposed to infer that START inherits from END.")

(eieio-defclass-autoload 'cogre-aggregate '(cogre-link) "cogre-uml" "This type of link indicates aggregation.\nThe `start' node is the owner of the aggregation, the `end' node is\nthe item being aggregated.\nThis is supposed to infer that START contains END.")

(autoload 'cogre-uml-enable-unicode "cogre-uml" "\
Enable use of UNICODE symbols to create COGRE graphs.
Inheritance uses math triangle on page 25a0.
Aggregation uses math square on edge 25a0.
Line-drawing uses line-drawing codes on page 2500.
See http://unicode.org/charts/symbols.html.

The unicode symbols can be differing widths.  This will make the
cogre chart a little screwy somteims.  Your mileage may vary.

\(fn)" t nil)

(autoload 'cogre-uml-sort-for-lineage "cogre-uml" "\
Sort the current graph G for determining inheritance lineage.
Return it as a list of lists.  Each entry is of the form:
  ( NODE PARENT1 PARENT2 ... PARENTN)

\(fn G)" t nil)

;;;***

;;;### (autoloads (cogre-utest-quick-class cogre-utest) "cogre-utest"
;;;;;;  "cedet/cogre/cogre-utest.el" (18912 16027))
;;; Generated autoloads from cedet/cogre/cogre-utest.el

(autoload 'cogre-utest "cogre-utest" "\
Unit test Various aspects of COGRE.

\(fn)" t nil)

(autoload 'cogre-utest-quick-class "cogre-utest" "\
Test the quick-class function.

\(fn)" t nil)

;;;***

;;;### (autoloads (col-highlight-flash col-highlight-set-interval
;;;;;;  col-highlight-toggle-when-idle column-highlight-mode col-highlight-period
;;;;;;  col-highlight-vline-face-flag column-highlight) "col-highlight"
;;;;;;  "col-highlight.el" (19804 63391))
;;; Generated autoloads from col-highlight.el

(let ((loads (get 'column-highlight 'custom-loads))) (if (member '"col-highlight" loads) nil (put 'column-highlight 'custom-loads (cons '"col-highlight" loads))))

(defvar col-highlight-vline-face-flag t "\
*Non-nil means `column-highlight-mode' uses `col-highlight-face'.
nil means that it uses `vline-face'.")

(custom-autoload 'col-highlight-vline-face-flag "col-highlight" t)

(defvar col-highlight-period 1 "\
*Number of seconds to highlight the current column.")

(custom-autoload 'col-highlight-period "col-highlight" t)

(defface col-highlight '((t (:background "SlateGray3"))) "\
*Face for current-column highlighting by `column-highlight-mode'.
Not used if `col-highlight-vline-face-flag' is nil." :group (quote column-highlight) :group (quote faces))

(defvar column-highlight-mode nil "\
Non-nil if Column-Highlight mode is enabled.
See the command `column-highlight-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `column-highlight-mode'.")

(custom-autoload 'column-highlight-mode "col-highlight" nil)

(autoload 'column-highlight-mode "col-highlight" "\
Toggle highlighting the current column.
With ARG, turn column highlighting on if and only if ARG is positive.

Column-Highlight mode uses the functions
`col-highlight-unhighlight' and `col-highlight-highlight'
on `pre-command-hook' and `post-command-hook'.

\(fn &optional ARG)" t nil)

(defalias 'toggle-highlight-column-when-idle 'col-highlight-toggle-when-idle)

(autoload 'col-highlight-toggle-when-idle "col-highlight" "\
Turn on or off highlighting the current column when Emacs is idle.
With prefix argument, turn on if ARG > 0; else turn off.

\(fn &optional ARG)" t nil)

(autoload 'col-highlight-set-interval "col-highlight" "\
Set wait until highlight current column when Emacs is idle.
Whenever Emacs is idle for this many seconds, the current column
will be highlighted in the face that is the value of variable
`col-highlight-face'.

To turn on or off automatically highlighting the current column
when Emacs is idle, use `\\[toggle-highlight-column-when-idle].

\(fn SECS)" t nil)

(defalias 'flash-column-highlight 'col-highlight-flash)

(autoload 'col-highlight-flash "col-highlight" "\
Highlight the current column for `col-highlight-period' seconds.
With a prefix argument, highlight for that many seconds.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (color-theme-initialize color-theme-submit color-theme-install
;;;;;;  color-theme-compare color-theme-make-snapshot color-theme-analyze-defun
;;;;;;  color-theme-print color-theme-install-at-point-for-current-frame
;;;;;;  color-theme-install-at-mouse color-theme-describe color-theme-select)
;;;;;;  "color-theme" "color-theme-6.6.0/color-theme.el" (17528 52192))
;;; Generated autoloads from color-theme-6.6.0/color-theme.el

(autoload 'color-theme-select "color-theme" "\
Displays a special buffer for selecting and installing a color theme.
With optional prefix ARG, this buffer will include color theme libraries
as well.  A color theme library is in itself not complete, it must be
used as part of another color theme to be useful.  Thus, color theme
libraries are mainly useful for color theme authors.

\(fn &optional ARG)" t nil)

(autoload 'color-theme-describe "color-theme" "\
Describe color theme listed at point.
This shows the documentation of the value of text-property color-theme
at point.  The text-property color-theme should be a color theme
function.  See `color-themes'.

\(fn)" t nil)

(autoload 'color-theme-install-at-mouse "color-theme" "\
Install color theme clicked upon using the mouse.
First argument EVENT is used to set point.  Then
`color-theme-install-at-point' is called.

\(fn EVENT)" t nil)

(autoload 'color-theme-install-at-point-for-current-frame "color-theme" "\
Install color theme at point for current frame only.
Binds `color-theme-is-global' to nil and calls
`color-theme-install-at-point'.

\(fn)" t nil)

(autoload 'color-theme-print "color-theme" "\
Print the current color theme function.

You can contribute this function to <URL:news:gnu.emacs.sources> or
paste it into your .emacs file and call it.  That should recreate all
the settings necessary for your color theme.

Example:

    (require 'color-theme)
    (defun my-color-theme ()
      \"Color theme by Alex Schroeder, created 2000-05-17.\"
      (interactive)
      (color-theme-install
       '(...
	 ...
	 ...)))
    (my-color-theme)

If you want to use a specific color theme function, you can call the
color theme function in your .emacs directly.

Example:

    (require 'color-theme)
    (color-theme-gnome2)

\(fn &optional BUF)" t nil)

(autoload 'color-theme-analyze-defun "color-theme" "\
Once you have a color-theme printed, check for missing faces.
This is used by maintainers who receive a color-theme submission
and want to make sure it follows the guidelines by the color-theme
author.

\(fn)" t nil)

(autoload 'color-theme-make-snapshot "color-theme" "\
Return the definition of the current color-theme.
The function returned will recreate the color-theme in use at the moment.

\(fn)" nil nil)

(autoload 'color-theme-compare "color-theme" "\
Compare two color themes.
This will print the differences between installing THEME-A and
installing THEME-B.  Note that the order is important: If a face is
defined in THEME-A and not in THEME-B, then this will not show up as a
difference, because there is no reset before installing THEME-B.  If a
face is defined in THEME-B and not in THEME-A, then this will show up as
a difference.

\(fn THEME-A THEME-B)" t nil)

(autoload 'color-theme-install "color-theme" "\
Install a color theme defined by frame parameters, variables and faces.

The theme is installed for all present and future frames; any missing
faces are created.  See `color-theme-install-faces'.

THEME is a color theme definition.  See below for more information.

If you want to install a color theme from your .emacs, use the output
generated by `color-theme-print'.  This produces color theme function
which you can copy to your .emacs.

A color theme definition is a list:
\([FUNCTION] FRAME-PARAMETERS VARIABLE-SETTINGS FACE-DEFINITIONS)

FUNCTION is the color theme function which called `color-theme-install'.
This is no longer used.  There was a time when this package supported
automatic factoring of color themes.  This has been abandoned.

FRAME-PARAMETERS is an alist of frame parameters.  These are installed
with `color-theme-install-frame-params'.  These are installed last such
that any changes to the default face can be changed by the frame
parameters.

VARIABLE-DEFINITIONS is an alist of variable settings.  These are
installed with `color-theme-install-variables'.

FACE-DEFINITIONS is an alist of face definitions.  These are installed
with `color-theme-install-faces'.

If `color-theme-is-cumulative' is nil, a color theme will undo face and
frame-parameter settings of previous color themes.

\(fn THEME)" nil nil)

(autoload 'color-theme-submit "color-theme" "\
Submit your color-theme to the maintainer.

\(fn)" t nil)

(autoload 'color-theme-initialize "color-theme" "\
Initialize the color theme package by loading color-theme-libraries.

\(fn)" t nil)

;;;***

;;;### (autoloads (compilation-recenter-end-enable) "compilation-recenter-end"
;;;;;;  "compilation-recenter-end.el" (18891 42871))
;;; Generated autoloads from compilation-recenter-end.el

(autoload 'compilation-recenter-end-enable "compilation-recenter-end" "\
Enable recentring of compilation windows at finish.
This function adds `compilation-recenter-end-at-finish' to
`compilation-finish-functions' (for Emacs 21 and up) or sets it
into `compilation-finish-function' (otherwise).  This is a global
change, affecting all compilation-mode buffers.

If you want multiple finish functions and only have an old Emacs
with the single `compilation-finish-function', you might try your
own defvar of `compilation-finish-functions' and set the single
function to call those.  `compilation-recenter-end-enable' here
will notice any `compilation-finish-functions' and use that.

\(fn)" nil nil)

(custom-add-option 'compilation-mode-hook 'compilation-recenter-end-enable)

;;;***

;;;### (autoloads (crosshairs-unhighlight crosshairs-highlight crosshairs
;;;;;;  crosshairs-flash crosshairs-toggle-when-idle crosshairs-mode
;;;;;;  crosshairs-vline-same-face-flag crosshairs-overlay-priority
;;;;;;  crosshairs) "crosshairs" "crosshairs.el" (19804 63376))
;;; Generated autoloads from crosshairs.el

(let ((loads (get 'crosshairs 'custom-loads))) (if (member '"crosshairs" loads) nil (put 'crosshairs 'custom-loads (cons '"crosshairs" loads))))

(defvar crosshairs-overlay-priority nil "\
*Priority to use for overlay `global-hl-line-overlay'.")

(custom-autoload 'crosshairs-overlay-priority "crosshairs" t)

(defvar crosshairs-vline-same-face-flag t "\
*Non-nil means use face `hl-line' for column highlighting also.
nil means highlight the column according to the value of `vline-style'
and face `vline'.")

(custom-autoload 'crosshairs-vline-same-face-flag "crosshairs" t)

(defvar crosshairs-mode nil "\
Non-nil if Crosshairs mode is enabled.
See the command `crosshairs-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `crosshairs-mode'.")

(custom-autoload 'crosshairs-mode "crosshairs" nil)

(autoload 'crosshairs-mode "crosshairs" "\
Toggle highlighting the current line and column.
With ARG, turn highlighting on if and only if ARG is positive.

\(fn &optional ARG)" t nil)

(defalias 'toggle-crosshairs-when-idle 'crosshairs-toggle-when-idle)

(autoload 'crosshairs-toggle-when-idle "crosshairs" "\
Toggle highlighting the current line and column when Emacs is idle.
With prefix argument, turn on if ARG > 0; else turn off.
You can use commands `col-highlight-set-interval' and
`hl-line-when-idle-interval' to change the idle times.

\(fn &optional ARG)" t nil)

(defalias 'flash-crosshairs 'crosshairs-flash)

(autoload 'crosshairs-flash "crosshairs" "\
Highlight the current line and column temporarily.
Highlight the line for `hl-line-flash-show-period' and the column for
`column-show-period' seconds.  With prefix argument SECONDS, highlight
both for SECONDS seconds.

\(fn &optional SECONDS)" t nil)

(autoload 'crosshairs "crosshairs" "\
Highlight current position with crosshairs.
With no prefix arg, highlighting turns off at the next command.
With a prefix arg, highlighting stays on until you toggle it off using
`crosshairs-mode'.

\(fn &optional MODALP)" t nil)

(autoload 'crosshairs-highlight "crosshairs" "\
Echo current position and highlight it with crosshairs.
If optional arg MODE is `line-only', then highlight only the line.
If optional arg MODE is `col-only', then highlight only the column.
 Interactively:
  A non-negative prefix argument uses MODE `line-only'.
  A negative prefix argument uses MODE `col-only'.

Optional arg NOMSG non-nil means show no message.

If the current buffer is not the same as the value of `orig-buff',
then indicate the buffer, as well as the position.  Variable
`orig-buff' is not bound here; if you want to take advantage of this
feature in your code, then bind it.

Return current position as a marker.

\(fn &optional MODE NOMSG)" t nil)

(autoload 'crosshairs-unhighlight "crosshairs" "\
Turn off crosshairs highlighting of current position.
Optional arg nil means do nothing if this event is a frame switch.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (css-mode) "css-mode" "css-mode.el" (18003 35431))
;;; Generated autoloads from css-mode.el

(autoload 'css-mode "css-mode" "\
Major mode for editing CSS source code.

Key bindings:

\\{css-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (ctypes-read-file ctypes-auto-parse-mode ctypes-file
;;;;;;  ctypes-dir ctypes-tags ctypes-all-buffers ctypes-buffer ctypes-define-type-in-mode
;;;;;;  ctypes-define-type) "ctypes" "ctypes.el" (18227 38128))
;;; Generated autoloads from ctypes.el

(autoload 'ctypes-define-type "ctypes" "\
Add a new TYPE to current major mode and inform font-lock.

When preceded by C-u the display is not updated.

Return non-nil if the type was not known before.

\(fn TYPE &optional DELAY-ACTION MODE)" t nil)

(autoload 'ctypes-define-type-in-mode "ctypes" "\
Add TYPE to major mode MODE and inform font-lock.

When preceded by C-u the display is not updated.

\(This function is designed for interactive use, please call
`ctypes-define-type' from Lisp programs.)

\(fn TYPE &optional DELAY-ACTION MODE)" t nil)

(autoload 'ctypes-buffer "ctypes" "\
Search for types in buffer, inform font-lock if any is found.

When preceded by C-u the action is not performed.

Return non-nil if new types are found.

\(fn &optional BUF DELAY-ACTION MODE)" t nil)

(autoload 'ctypes-all-buffers "ctypes" "\
Search for types in all buffers, inform font-lock about all discoveries.

When preceded by C-u the display is not updated.

Return non-nil if new types are found.

\(fn &optional DELAY-ACTION)" t nil)

(autoload 'ctypes-tags "ctypes" "\
Search for types in files in the visited TAGS table.
Should no tags table be visited, the user will be prompted for a new.

When preceded by C-u the display is not updated.

Return non-nil if new types are found.

\(fn &optional DELAY-ACTION)" t nil)

(autoload 'ctypes-dir "ctypes" "\
Search for types in files in a directory hierarchy.

See variable `ctypes-dir-read-file' for a description of which files
are opened during scanning, and how you can change the behavior.

When preceded by C-u the display is not updated.

Return non-nil if new types are found.

\(fn &optional DIR DELAY-ACTION)" t nil)

(autoload 'ctypes-file "ctypes" "\
Search for types in file FILE.
Should FILE not be loaded it is read into a temporary buffer.

Return mode of file, if new types was found.

\(fn FILE &optional DELAY-ACTION)" t nil)

(autoload 'ctypes-auto-parse-mode "ctypes" "\
Toggle CTypes auto parse mode; search all new buffers for types.
With arg, turn types Auto Mode on if and only if arg is positive.

This a global minor mode, it does not have a private keymap, nor does
it add itself to the mode line.

Place the following in your startup file to enable this feature in
future sessions:

    (require 'ctypes)
    (ctypes-auto-parse-mode 1)

When activated, the functions in the hook `ctypes-auto-parse-mode-hook'
is called with no args.

\(fn &optional ARG)" t nil)

(autoload 'ctypes-read-file "ctypes" "\
Load types previously saved with `ctypes-write-file'.
The name of the file is given by the optional argument FILE.
Should no file name be given the value of the variable `ctypes-file-name'
is used.

Please note that the types read will be added to the current types.

When preceded by C-u the display is not updated.

The third argument, NO-ERROR, determines whether or not we should
raise an error if there should be any problem loading the file.

Should the fourth argument, QUIETLY, be non-nil no messages are
generated when the file is loaded.

Return non-nil if new types are found.

\(fn &optional FILE DELAY-ACTION NO-ERROR QUIETLY)" t nil)

;;;***

;;;### (autoloads (cucumber-compilation-run cucumber-compilation-this-scenario
;;;;;;  cucumber-compilation-this-buffer) "cucumber-mode-compilation"
;;;;;;  "rinari/util/cucumber-mode-compilation.el" (19809 16085))
;;; Generated autoloads from rinari/util/cucumber-mode-compilation.el

(autoload 'cucumber-compilation-this-buffer "cucumber-mode-compilation" "\
Run the current buffer's scenarios through cucumber.

\(fn)" t nil)

(autoload 'cucumber-compilation-this-scenario "cucumber-mode-compilation" "\
Run the scenario at point through cucumber.

\(fn)" t nil)

(autoload 'cucumber-compilation-run "cucumber-mode-compilation" "\
Run a cucumber process, dumping output to a compilation buffer.

\(fn CMD)" t nil)

;;;***

;;;### (autoloads (cycle-buffer-toggle-interesting cycle-buffer-backward-permissive
;;;;;;  cycle-buffer-permissive cycle-buffer-backward cycle-buffer)
;;;;;;  "cycle-buffer" "cycle-buffer.el" (19828 52155))
;;; Generated autoloads from cycle-buffer.el

(autoload 'cycle-buffer "cycle-buffer" "\
Switch to the next buffer on the buffer list without prompting.
Successive invocations select buffers further down on the buffer list.
A prefix argument specifies the DISTANCE to skip, negative moves back.

\(fn &optional DISTANCE PERMISSIVE)" t nil)

(autoload 'cycle-buffer-backward "cycle-buffer" "\
Switch to the previous buffer in the buffer list.

\(fn &optional DISTANCE)" t nil)

(autoload 'cycle-buffer-permissive "cycle-buffer" "\
Switch to the next buffer, allowing more buffers (*bufname* by default).

\(fn &optional DISTANCE)" t nil)

(autoload 'cycle-buffer-backward-permissive "cycle-buffer" "\
Switch to the previous buffer, allowing more buffers (*bufname* by default).

\(fn &optional DISTANCE)" t nil)

(autoload 'cycle-buffer-toggle-interesting "cycle-buffer" "\
Toggle the value of cycle-buffer-interesting for the current buffer.
With positive arg set it, with non-positive arg reset it. A buffer is only
considered by cycle-buffer when cycle-buffer-interesting is t.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (data-debug-eval-expression data-debug-edebug-expr
;;;;;;  data-debug-show-stuff data-debug-new-buffer data-debug-mode
;;;;;;  data-debug-insert-thing data-debug-insert-stuff-vector data-debug-insert-stuff-list
;;;;;;  data-debug-insert-widget-properties data-debug-insert-hash-table
;;;;;;  data-debug-insert-property-list) "data-debug" "cedet/common/data-debug.el"
;;;;;;  (19477 27308))
;;; Generated autoloads from cedet/common/data-debug.el

(autoload 'data-debug-insert-property-list "data-debug" "\
Insert the property list PROPLIST.
Each line starts with PREFIX.
The attributes belong to the tag PARENT.

\(fn PROPLIST PREFIX &optional PARENT)" nil nil)

(autoload 'data-debug-insert-hash-table "data-debug" "\
Insert the contents of HASH-TABLE inserting PREFIX before each element.

\(fn HASH-TABLE PREFIX)" nil nil)

(autoload 'data-debug-insert-widget-properties "data-debug" "\
Insert the contents of WIDGET inserting PREFIX before each element.

\(fn WIDGET PREFIX)" nil nil)

(autoload 'data-debug-insert-stuff-list "data-debug" "\
Insert all the parts of STUFFLIST.
PREFIX specifies what to insert at the start of each line.

\(fn STUFFLIST PREFIX)" nil nil)

(autoload 'data-debug-insert-stuff-vector "data-debug" "\
Insert all the parts of STUFFVECTOR.
PREFIX specifies what to insert at the start of each line.

\(fn STUFFVECTOR PREFIX)" nil nil)

(autoload 'data-debug-insert-thing "data-debug" "\
Insert THING with PREFIX.
PREBUTTONTEXT is some text to insert between prefix and the thing
that is not included in the indentation calculation of any children.
If PARENT is non-nil, it is somehow related as a parent to thing.

\(fn THING PREFIX PREBUTTONTEXT &optional PARENT)" nil nil)

(autoload 'data-debug-mode "data-debug" "\
Major-mode for the Analyzer debugger.

\\{data-debug-map}

\(fn)" t nil)

(autoload 'data-debug-new-buffer "data-debug" "\
Create a new ddebug buffer with NAME.

\(fn NAME)" nil nil)

(autoload 'data-debug-show-stuff "data-debug" "\
Data debug STUFF in a buffer named *NAME DDebug*.

\(fn STUFF NAME)" nil nil)

(autoload 'data-debug-edebug-expr "data-debug" "\
Dump out the contents of some expression EXPR in edebug with ddebug.

\(fn EXPR)" t nil)

(autoload 'data-debug-eval-expression "data-debug" "\
Evaluate EXPR and display the value.
If the result is something simple, show it in the echo area.
If the result is a list or vector, then use the data debugger to display it.

\(fn EXPR)" t nil)

;;;***

;;;### (autoloads (diredp-mouse-do-chown diredp-mouse-do-chgrp diredp-mouse-do-chmod
;;;;;;  diredp-mouse-do-load diredp-mouse-do-byte-compile diredp-mouse-do-compress
;;;;;;  diredp-mouse-do-grep diredp-mouse-do-print diredp-mouse-do-hardlink
;;;;;;  diredp-mouse-do-symlink diredp-mouse-do-shell-command diredp-mouse-do-delete
;;;;;;  diredp-mouse-downcase diredp-mouse-upcase diredp-mouse-do-rename
;;;;;;  diredp-mouse-do-copy diredp-mouse-flag-file-deletion diredp-mouse-mark/unmark-mark-region-files
;;;;;;  diredp-mouse-mark-region-files diredp-mouse-mark/unmark diredp-mouse-unmark
;;;;;;  diredp-mouse-mark diredp-mouse-backup-diff diredp-mouse-diff
;;;;;;  diredp-mouse-ediff diredp-mouse-view-file diredp-mouse-find-file
;;;;;;  dired-mouse-find-file-other-window diredp-mouse-find-file-other-frame
;;;;;;  diredp-find-file-other-frame diredp-mouse-3-menu diredp-toggle-marks-in-region
;;;;;;  diredp-flag-region-files-for-deletion diredp-unmark-region-files
;;;;;;  diredp-mark-region-files dired-mark-sexp diredp-chown-this-file
;;;;;;  diredp-chgrp-this-file diredp-chmod-this-file diredp-load-this-file
;;;;;;  diredp-byte-compile-this-file diredp-shell-command-this-file
;;;;;;  diredp-compress-this-file diredp-grep-this-file diredp-print-this-file
;;;;;;  diredp-hardlink-this-file diredp-symlink-this-file diredp-relsymlink-this-file
;;;;;;  diredp-copy-this-file diredp-rename-this-file diredp-upcase-this-file
;;;;;;  diredp-downcase-this-file diredp-capitalize-this-file diredp-delete-this-file
;;;;;;  diredp-capitalize dired-do-delete dired-do-flagged-delete
;;;;;;  dired-goto-file dired-up-directory dired-do-find-marked-files
;;;;;;  dired-maybe-insert-subdir diredp-w32-drives diredp-w32-list-mapped-drives
;;;;;;  diredp-w32-drives-mode dired-do-load dired-do-byte-compile
;;;;;;  dired-do-compress diredp-ediff diredp-omit-unmarked diredp-omit-marked
;;;;;;  toggle-dired-find-file-reuse-dir diredp-mouse-find-file-reuse-dir-buffer
;;;;;;  diredp-find-file-reuse-dir-buffer diredp-mouse-do-bookmark
;;;;;;  diredp-do-bookmark diredp-do-bookmark-in-bookmark-file diredp-set-bookmark-file-bookmark-for-marked
;;;;;;  diredp-mark/unmark-extension diredp-marked-other-window diredp-marked
;;;;;;  diredp-fileset diredp-dired-union-other-window diredp-dired-union
;;;;;;  diredp-dired-files-other-window diredp-dired-files diredp-w32-local-drives
;;;;;;  diff-switches) "dired+" "dired+.el" (19804 60911))
;;; Generated autoloads from dired+.el

(defvar diff-switches "-c" "\
*A string or list of strings specifying switches to be passed to diff.")

(custom-autoload 'diff-switches "dired+" t)

(defvar diredp-w32-local-drives '(("C:" "Local disk")) "\
Local MS Windows drives that you want to use for `diredp-w32-drives'.
Each entry is a list (DRIVE DESCRIPTION), where DRIVE is the drive
name and DESCRIPTION describes DRIVE.")

(custom-autoload 'diredp-w32-local-drives "dired+" t)

(autoload 'diredp-dired-files "dired+" "\
Like `dired', but non-positive prefix arg prompts for files to list.
This is the same as `dired' unless you use a non-positive prefix arg.
In that case, you are prompted for names of files and directories to
list, and then you are prompted for the name of the Dired buffer that
lists them.  Use `C-g' when you are done entering file names to list.

In all cases, when inputting a file or directory name you can use
shell wildcards.

\(fn ARG &optional SWITCHES)" t nil)

(autoload 'diredp-dired-files-other-window "dired+" "\
Same as `diredp-dired-files' except uses another window.

\(fn ARG &optional SWITCHES)" t nil)

(autoload 'diredp-dired-union "dired+" "\
Create a Dired buffer that is the union of some existing Dired buffers.
With a prefix arg, read `ls' switches.
You are prompted for the Dired buffers.  Use `C-g' when done choosing
them.  Then you are prompted for the name of the new Dired buffer.
Its `default-directory' is the same as the `default-directory' before
invoking the command.

The selected Dired listings are included in the order that you choose
them, and each entry is listed only once in the new Dired buffer.  The
new Dired listing respects the markings, subdirectory insertions, and
hidden subdirectories of the selected Dired listings.

However, in case of conflict between marked or unmarked status for the
same entry, the entry is marked.  Similarly, in case of conflict over
an included subdirectory between it being hidden or shown, it is
hidden, but its contained files are also listed.

\(fn DIRBUFS &optional SWITCHES)" t nil)

(autoload 'diredp-dired-union-other-window "dired+" "\
Same as `diredp-dired-union' but uses another window.

\(fn DIRBUFS &optional SWITCHES)" t nil)

(autoload 'diredp-fileset "dired+" "\
Open Dired on the files in fileset FLSET-NAME.

\(fn FLSET-NAME)" t nil)

(autoload 'diredp-marked "dired+" "\
Open Dired on only the marked files or the next N files.
With a non-zero numeric prefix arg N, use the next abs(N) files.
A plain (`C-u'), zero, or negative prefix arg prompts for listing
switches as in command `dired'.

Note that the marked files can include files in inserted
subdirectories, so the Dired buffer that is opened can contain files
from multiple directories in the same tree.

\(fn DIRNAME &optional N SWITCHES)" t nil)

(autoload 'diredp-marked-other-window "dired+" "\
Same as `diredp-marked', but uses a different window.

\(fn DIRNAME &optional N SWITCHES)" t nil)

(autoload 'diredp-mark/unmark-extension "dired+" "\
Mark all files with a certain EXTENSION for use in later commands.
A `.' is not automatically prepended to the string entered.
Non-nil prefix argument UNMARK-P means unmark instead of mark.

\(fn EXTENSION &optional UNMARK-P)" t nil)

(autoload 'diredp-set-bookmark-file-bookmark-for-marked "dired+" "\
Bookmark the marked files and create a bookmark-file bookmark for them.
Jumping to the bookmark-file bookmark loads the set of file bookmarks.

Each bookmark name is PREFIX followed by the relative file name.
Interactively, you are prompted for PREFIX.
The bookmarked position is the beginning of the file.

A prefix argument ARG specifies files to use instead of those marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories.

You are also prompted for the bookmark file, BOOKMARK-FILE.  The
default is `.emacs.bmk' in the current directory, but you can enter
any file name, anywhere.

The marked-file bookmarks are added to file BOOKMARK-FILE, but this
command does not make BOOKMARK-FILE the current bookmark file.  To
make it current, just jump to the bookmark-file bookmark created by
this command.  That bookmark (which bookmarks BOOKMARK-FILE) is
defined in that current bookmark file.

Example:

 Bookmark file `~/.emacs.bmk' is current before invoking this command.
 The current (Dired) directory is `/foo/bar'.
 The marked files are bookmarked in the (possibly new) bookmark file
   `/foo/bar/.emacs.bmk'.
 The bookmarks for the marked files have names prefixed by `FOOBAR '.
 The name of the bookmark-file bookmark is `Foobar Files'.
 Bookmark `Foobar Files' is itself in bookmark file `~/.emacs.bmk'.
 Bookmark file `~/.emacs.bmk' is current after invoking this command.

You are prompted for the name of the bookmark-file bookmark, the
BOOKMARK-FILE for the marked-file bookmarks, and a PREFIX string for
each of the marked-file bookmarks.

See also command `diredp-do-bookmark-in-bookmark-file'.

\(fn BOOKMARK-FILE PREFIX &optional ARG)" t nil)

(autoload 'diredp-do-bookmark-in-bookmark-file "dired+" "\
Bookmark the marked files in BOOKMARK-FILE and save BOOKMARK-FILE.
You are prompted for BOOKMARK-FILE.  The default is `.emacs.bmk' in
the current directory, but you can enter any file name, anywhere.

The marked files are bookmarked in file BOOKMARK-FILE, but this
command does not make BOOKMARK-FILE the current bookmark file.  To
make it current, use `\\[bmkp-switch-bookmark-file]' (`bmkp-switch-bookmark-file').

Each bookmark name is PREFIX followed by the relative file name.
Interactively, you are prompted for PREFIX.
The bookmarked position is the beginning of the file.

A prefix argument ARG specifies files to use instead of those marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories.

See also command `diredp-set-bookmark-file-bookmark-for-marked'.

Non-interactively, non-nil BFILE-BOOKMARKP means create a
bookmark-file bookmark for BOOKMARK-FILE.

\(fn BOOKMARK-FILE PREFIX &optional ARG BFILE-BOOKMARKP)" t nil)

(autoload 'diredp-do-bookmark "dired+" "\
Bookmark the marked (or the next prefix argument) files.
Each bookmark name is PREFIX followed by the relative file name.
Interactively, you are prompted for the PREFIX.
The bookmarked position is the beginning of the file.

A prefix argument ARG specifies files to use instead of those marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories.

\(fn PREFIX &optional ARG)" t nil)

(autoload 'diredp-mouse-do-bookmark "dired+" "\
In Dired, bookmark this file.

\(fn EVENT &optional ARG)" t nil)

(autoload 'diredp-find-file-reuse-dir-buffer "dired+" "\
Like `dired-find-file', but reuse Dired buffers.
Unlike `dired-find-alternate-file' this does not use
`find-alternate-file' if the target is not a directory.

\(fn)" t nil)

(autoload 'diredp-mouse-find-file-reuse-dir-buffer "dired+" "\
Like `diredp-mouse-find-file', but reuse Dired buffers.
Unlike `dired-find-alternate-file' this does not use
`find-alternate-file' if the target is not a directory.

\(fn EVENT)" t nil)

(defalias 'diredp-toggle-find-file-reuse-dir 'toggle-dired-find-file-reuse-dir)

(autoload 'toggle-dired-find-file-reuse-dir "dired+" "\
Toggle whether Dired `find-file' commands reuse directories.
A prefix arg specifies directly whether or not to reuse.
 If its numeric value is non-negative then reuse; else do not reuse.

To set the behavior as a preference (default behavior), put this in
your ~/.emacs, where VALUE is 1 to reuse or -1 to not reuse:

 (toggle-dired-find-file-reuse-dir VALUE)

\(fn FORCE-P)" t nil)

(autoload 'diredp-omit-marked "dired+" "\
Omit lines of marked files.  Return the number of lines omitted.

\(fn)" t nil)

(autoload 'diredp-omit-unmarked "dired+" "\
Omit lines of unmarked files.  Return the number of lines omitted.

\(fn)" t nil)

(autoload 'diredp-ediff "dired+" "\
Compare file at cursor with file FILE2 using `ediff'.
FILE2 defaults to the file at the cursor as well.  If you enter just a
directory name for FILE2, then the file at the cursor is compared with
a file of the same name in that directory.  FILE2 is the second file
given to `ediff'; the file at the cursor is the first.

\(fn FILE2)" t nil)

(autoload 'dired-do-compress "dired+" "\
Compress or uncompress marked (or next prefix argument) files.
A prefix argument ARG specifies files to use instead of marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories.

\(fn &optional ARG)" t nil)

(autoload 'dired-do-byte-compile "dired+" "\
Byte compile marked (or next prefix argument) Emacs Lisp files.
A prefix argument ARG specifies files to use instead of marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories.

\(fn &optional ARG)" t nil)

(autoload 'dired-do-load "dired+" "\
Load the marked (or next prefix argument) Emacs Lisp files.
A prefix argument ARG specifies files to use instead of marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories.

\(fn &optional ARG)" t nil)

(autoload 'diredp-w32-drives-mode "dired+" "\
Open Dired for an MS Windows drive (local or remote).

\(fn)" t nil)

(autoload 'diredp-w32-list-mapped-drives "dired+" "\
List network connection information for shared MS Windows resources.
This just invokes the Windows `NET USE' command.

\(fn)" t nil)

(autoload 'diredp-w32-drives "dired+" "\
Visit a list of MS Windows drives for use by Dired.
With a prefix argument use another window for the list.
In the list, use `mouse-2' or `RET' to open Dired for a given drive.

The drives listed are the remote drives currently available, as
determined by the Windows command `NET USE', plus the local drives
specified by option `diredp-w32-local-drives', which you can
customize.

Note: When you are in Dired at the root of a drive (e.g. directory
      `c:/'), command `dired-up-directory' invokes this command.
      So you can use `\\[dired-up-directory]' to go up to the list of drives.

\(fn &optional OTHER-WINDOW-P)" t nil)

(autoload 'dired-maybe-insert-subdir "dired+" "\
Move to Dired subdirectory line or subdirectory listing.
This bounces you back and forth between a subdirectory line and its
inserted listing header line.  Using it on a non-directory line in a
subdirectory listing acts the same as using it on the subdirectory
header line.

* If on a subdirectory line, then go to the subdirectory's listing,
  creating it if not yet present.

* If on a subdirectory listing header line or a non-directory file in
  a subdirectory listing, then go to the line for the subdirectory in
  the parent directory listing.

* If on a non-directory file in the top Dired directory listing, do
  nothing.

Subdirectories are listed in the same position as for `ls -lR' output.

With a prefix arg, you can edit the `ls' switches used for this
listing.  Add `R' to the switches to expand the directory tree under a
subdirectory.

Dired remembers the switches you specify with a prefix arg, so
reverting the buffer does not reset them.  However, you might
sometimes need to reset some subdirectory switches after a
`dired-undo'.  You can reset all subdirectory switches to the
default value using \\<dired-mode-map>\\[dired-reset-subdir-switches].  See Info node
`(emacs)Subdir switches' for more details.

\(fn DIRNAME &optional SWITCHES NO-ERROR-IF-NOT-DIR-P)" t nil)

(autoload 'dired-do-find-marked-files "dired+" "\
Find marked files, displaying all of them simultaneously.
With a prefix ARG >= 0, just find files but do not select them.

If no prefix ARG, and variable `pop-up-frames' is non-nil, or
if prefix ARG < 0, then each file is displayed in a separate frame.

Otherwise (no prefix ARG and nil `pop-up-frames'), the current window
is split across all marked files, as evenly as possible.  Remaining
lines go to the bottom-most window.  The number of files that can be
displayed this way is restricted by the height of the current window
and `window-min-height'.

A prefix argument also behaves according to the ARG argument of
`dired-get-marked-files'.  In particular, `C-u C-u' operates on all
files in the Dired buffer.

To keep the Dired buffer displayed, type \\[split-window-vertically] first.
To display just the marked files, type \\[delete-other-windows] first.

\(fn &optional ARG)" t nil)

(autoload 'dired-up-directory "dired+" "\
Run Dired on parent directory of current directory.
Find the parent directory either in this buffer or another buffer.
Creates a buffer if necessary.

On MS Windows, if you already at the root directory, invoke
`diredp-w32-drives' to visit a navigable list of Windows drives.

\(fn &optional OTHER-WINDOW)" t nil)

(autoload 'dired-goto-file "dired+" "\
Go to line describing file FILE in this dired buffer.

\(fn FILE)" t nil)

(autoload 'dired-do-flagged-delete "dired+" "\
In Dired, delete the files flagged for deletion.
NOTE: This deletes flagged, not marked, files.
If arg NO-MSG is non-nil, no message is displayed.

User option `dired-recursive-deletes' controls whether deletion of
non-empty directories is allowed.

\(fn &optional NO-MSG)" t nil)

(autoload 'dired-do-delete "dired+" "\
Delete all marked (or next ARG) files.
NOTE: This deletes marked, not flagged, files.
`dired-recursive-deletes' controls whether deletion of
non-empty directories is allowed.

\(fn &optional ARG)" t nil)

(autoload 'diredp-capitalize "dired+" "\
Rename all marked (or next ARG) files by capitilizing them.
This gives the file name(s) a first character in upper case and the
rest lower case.

\(fn &optional ARG)" t nil)

(autoload 'diredp-delete-this-file "dired+" "\
In dired, delete the file on the cursor line, upon confirmation.

\(fn)" t nil)

(autoload 'diredp-capitalize-this-file "dired+" "\
In dired, rename the file on the cursor line by capitilizing it.
This gives the file name a first character in upper case and the rest
lower case.

\(fn)" t nil)

(autoload 'diredp-downcase-this-file "dired+" "\
In dired, rename the file on the cursor line to lower case.

\(fn)" t nil)

(autoload 'diredp-upcase-this-file "dired+" "\
In dired, rename the file on the cursor line to upper case.

\(fn)" t nil)

(autoload 'diredp-rename-this-file "dired+" "\
In dired, rename the file on the cursor line.

\(fn)" t nil)

(autoload 'diredp-copy-this-file "dired+" "\
In dired, copy the file on the cursor line.

\(fn)" t nil)

(autoload 'diredp-relsymlink-this-file "dired+" "\
In dired, make a relative symbolic link to file on cursor line.

\(fn)" t nil)

(autoload 'diredp-symlink-this-file "dired+" "\
In dired, make a symbolic link to the file on the cursor line.

\(fn)" t nil)

(autoload 'diredp-hardlink-this-file "dired+" "\
In dired, add a name (hard link) to the file on the cursor line.

\(fn)" t nil)

(autoload 'diredp-print-this-file "dired+" "\
In dired, print the file on the cursor line.

\(fn)" t nil)

(autoload 'diredp-grep-this-file "dired+" "\
In dired, grep the file on the cursor line.

\(fn)" t nil)

(autoload 'diredp-compress-this-file "dired+" "\
In dired, compress or uncompress the file on the cursor line.

\(fn)" t nil)

(autoload 'diredp-shell-command-this-file "dired+" "\
In dired, run a shell COMMAND on the file on the cursor line.

\(fn COMMAND)" t nil)

(autoload 'diredp-byte-compile-this-file "dired+" "\
In dired, byte compile the (Lisp source) file on the cursor line.

\(fn)" t nil)

(autoload 'diredp-load-this-file "dired+" "\
In dired, load the file on the cursor line.

\(fn)" t nil)

(autoload 'diredp-chmod-this-file "dired+" "\
In dired, change the mode of the file on the cursor line.

\(fn)" t nil)

(autoload 'diredp-chgrp-this-file "dired+" "\
In dired, change the group of the file on the cursor line.

\(fn)" t nil)

(autoload 'diredp-chown-this-file "dired+" "\
In dired, change the owner of the file on the cursor line.

\(fn)" t nil)

(autoload 'dired-mark-sexp "dired+" "\
Mark files for which PREDICATE returns non-nil.
With non-nil prefix arg UNMARK-P, unmark those files instead.

PREDICATE is a lisp sexp that can refer to the following variables:

    `mode'   [string]  file permission bits, e.g. \"-rw-r--r--\"
    `nlink'  [integer] number of links to file
    `size'   [integer] file size in bytes
    `uid'    [string]  owner
    `gid'    [string]  group (If the gid is not displayed by `ls',
                       this will still be set (to the same as uid))
    `time'   [string]  the time that `ls' displays, e.g. \"Feb 12 14:17\"
    `name'   [string]  the name of the file
    `sym'    [string]  if file is a symbolic link, the linked-to name,
                       else \"\"
    `inode'  [integer] the inode of the file (only for `ls -i' output)
    `blks'   [integer] the size of the file for `ls -s' output
                       (ususally in blocks or, with `-k', in Kbytes)
Examples:
  Mark zero-length files: `(equal 0 size)'
  Mark files last modified on Feb 2: `(string-match \"Feb  2\" time)'
  Mark uncompiled Emacs Lisp files (`.el' file without a `.elc' file):
     First, dired just the source files: `dired *.el'.
     Then, use \\[dired-mark-sexp] with this sexp:
          (not (file-exists-p (concat name \"c\")))

\(fn PREDICATE &optional UNMARK-P)" t nil)

(autoload 'diredp-mark-region-files "dired+" "\
Mark all of the files in the current region (if it is active).
With non-nil prefix arg, unmark them instead.

\(fn &optional UNMARK-P)" t nil)

(autoload 'diredp-unmark-region-files "dired+" "\
Unmark all of the files in the current region (if it is active).
With non-nil prefix arg, mark them instead.

\(fn &optional MARK-P)" t nil)

(autoload 'diredp-flag-region-files-for-deletion "dired+" "\
Flag all of the files in the current region (if it is active) for deletion.

\(fn)" t nil)

(autoload 'diredp-toggle-marks-in-region "dired+" "\
Toggle marks in the region.

\(fn START END)" t nil)

(autoload 'diredp-mouse-3-menu "dired+" "\
Dired pop-up `mouse-3' menu, for files in selection or current line.

\(fn EVENT)" t nil)

(autoload 'diredp-find-file-other-frame "dired+" "\
In dired, visit this file or directory in another frame.

\(fn)" t nil)

(autoload 'diredp-mouse-find-file-other-frame "dired+" "\
In dired, visit file or directory clicked on in another frame.

\(fn EVENT)" t nil)

(autoload 'dired-mouse-find-file-other-window "dired+" "\
In dired, visit the file or directory name you click on.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-find-file "dired+" "\
Replace dired in its window by this file or directory.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-view-file "dired+" "\
Examine this file in view mode, returning to dired when done.
When file is a directory, show it in this buffer if it is inserted;
otherwise, display it in another buffer.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-ediff "dired+" "\
Compare this file (pointed by mouse) with file FILE2 using `ediff'.
FILE2 defaults to this file as well.  If you enter just a directory
name for FILE2, then this file is compared with a file of the same
name in that directory.  FILE2 is the second file given to `ediff';
this file is the first given to it.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-diff "dired+" "\
Compare this file (pointed by mouse) with file FILE2 using `diff'.
FILE2 defaults to the file at the mark.  This file is the first file
given to `diff'.  With prefix arg, prompt for second arg SWITCHES,
which are options for `diff'.

\(fn EVENT &optional SWITCHES)" t nil)

(autoload 'diredp-mouse-backup-diff "dired+" "\
Diff this file with its backup file or vice versa.
Use the latest backup, if there are several numerical backups.
If this file is a backup, diff it with its original.
The backup file is the first file given to `diff'.
With prefix arg, prompt for SWITCHES which are the options for `diff'.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-mark "dired+" "\
In dired, mark this file.
If on a subdir headerline, mark all its files except `.' and `..'.

Use \\[dired-unmark-all-files] to remove all marks,
and \\[dired-unmark] on a subdir to remove the marks in this subdir.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-unmark "dired+" "\
In dired, unmark this file.
If looking at a subdir, unmark all its files except `.' and `..'.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-mark/unmark "dired+" "\
Mark/unmark file or directory at mouse EVENT.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-mark-region-files "dired+" "\
Mark files between point and the mouse.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-mark/unmark-mark-region-files "dired+" "\
Mark/unmark file or mark files in region.
If the file the cursor is on is marked, then mark all files between it
 and the line clicked (included).
Otherwise (cursor's file is unmarked):
 If the file clicked is marked, then unmark it.
 If it is unmarked, then mark it.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-flag-file-deletion "dired+" "\
In dired, flag this file for deletion.
If on a subdir headerline, mark all its files except `.' and `..'.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-do-copy "dired+" "\
In dired, copy this file.
This normally preserves the last-modified date when copying.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-do-rename "dired+" "\
In dired, rename this file.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-upcase "dired+" "\
In dired, rename this file to upper case.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-downcase "dired+" "\
In dired, rename this file to lower case.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-do-delete "dired+" "\
In dired, delete this file, upon confirmation.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-do-shell-command "dired+" "\
Run a shell COMMAND on this file.
If there is output, it goes to a separate buffer.

No automatic redisplay of dired buffers is attempted, as there's no
telling what files the command may have changed.  Type
\\[dired-do-redisplay] to redisplay.

The shell command has the top level directory as working directory, so
output files usually are created there instead of in a subdir.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-do-symlink "dired+" "\
Make symbolic link to this file.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-do-hardlink "dired+" "\
Make hard link (alias) to this file.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-do-print "dired+" "\
Print this file.
Uses the shell command coming from variables `lpr-command' and
`lpr-switches' as default.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-do-grep "dired+" "\
Run grep against this file.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-do-compress "dired+" "\
Compress or uncompress this file.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-do-byte-compile "dired+" "\
Byte compile this file.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-do-load "dired+" "\
Load this Emacs Lisp file.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-do-chmod "dired+" "\
Change the mode of this file.
This calls chmod, so symbolic modes like `g+w' are allowed.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-do-chgrp "dired+" "\
Change the group of this file.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-do-chown "dired+" "\
Change the owner of this file.

\(fn EVENT)" t nil)

;;;***

;;;### (autoloads (dired-details-propagate-flag dired-details-hidden-string)
;;;;;;  "dired-details+" "dired-details+.el" (19806 53623))
;;; Generated autoloads from dired-details+.el

(defvar dired-details-hidden-string "" "\
*This string will be shown in place of file details and symbolic links.")

(custom-autoload 'dired-details-hidden-string "dired-details+" t)

(defvar dired-details-propagate-flag t "\
Non-nil means next Dired buffer should be displayed the same.
The last `dired-details-state' value set is used by the next Dired
buffer created.")

(custom-autoload 'dired-details-propagate-flag "dired-details+" t)

;;;***

;;;### (autoloads (doc-view) "doc-view" "doc-view.el" (18755 39836))
;;; Generated autoloads from doc-view.el

(autoload 'doc-view "doc-view" "\
Convert FILE to png and start viewing it.
If no FILE is given, query for on.
If this FILE is still in the cache, don't convert and use the
existing page files.  With prefix arg NO-CACHE, don't use the
cached files and convert anew.

\(fn NO-CACHE &optional FILE)" t nil)

;;;***

;;;### (autoloads (doctest-mode doctest-register-mmm-classes) "doctest-mode"
;;;;;;  "python-mode/doctest-mode.el" (19698 55469))
;;; Generated autoloads from python-mode/doctest-mode.el

(autoload 'doctest-register-mmm-classes "doctest-mode" "\
Register doctest's mmm classes, allowing doctest to be used as a
submode region in other major modes, such as python-mode and rst-mode.
Two classes are registered:

`doctest-docstring'

    Used to edit docstrings containing doctest examples in python-
    mode.  Docstring submode regions start and end with triple-quoted
    strings (\"\"\").  In order to avoid confusing start-string
    markers and end-string markers, all triple-quote strings in the
    buffer are treated as submode regions (even if they're not
    actually docstrings).  Use (C-c % C-d) to insert a new doctest-
    docstring region.  When `doctest-execute' (C-c C-c) is called
    inside a doctest-docstring region, it executes just the current
    docstring.  The globals for this execution are constructed by
    importing the current buffer's contents in Python.

`doctest-example'

    Used to edit doctest examples in text-editing modes, such as
    `rst-mode' or `text-mode'.  Docstring submode regions start with
    optionally indented prompts (>>>) and end with blank lines.  Use
    (C-c % C-e) to insert a new doctest-example region.  When
    `doctest-execute' (C-c C-c) is called inside a doctest-example
    region, it executes all examples in the buffer.

If ADD-MODE-EXT-CLASSES is true, then register the new classes in
`mmm-mode-ext-classes-alist', which will cause them to be used by
default in the following modes:

    doctest-docstring:  python-mode
    doctest-example:    rst-mode

If FIX-MMM-FONTIFY-REGION-BUG is true, then register a hook that will
fix a bug in `mmm-fontify-region' that affects some (but not all)
versions of emacs.  (See `doctest-fixed-mmm-fontify-region' for more
info.)

\(fn &optional ADD-MODE-EXT-CLASSES FIX-MMM-FONTIFY-REGION-BUG)" t nil)

(add-to-list 'auto-mode-alist '("\\.doctest$" . doctest-mode))

(autoload 'doctest-mode "doctest-mode" "\
A major mode for editing text files that contain Python
doctest examples.  Doctest is a testing framework for Python that
emulates an interactive session, and checks the result of each
command.  For more information, see the Python library reference:
<http://docs.python.org/lib/module-doctest.html>

`doctest-mode' defines three kinds of line, each of which is
treated differently:

  - 'Source lines' are lines consisting of a Python prompt
    ('>>>' or '...'), followed by source code.  Source lines are
    colored (similarly to `python-mode') and auto-indented.

  - 'Output lines' are non-blank lines immediately following
    source lines.  They are colored using several doctest-
    specific output faces.

  - 'Text lines' are any other lines.  They are not processed in
    any special way.

\\{doctest-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (eassist-list-methods eassist-switch-h-cpp) "eassist"
;;;;;;  "cedet/contrib/eassist.el" (19312 40567))
;;; Generated autoloads from cedet/contrib/eassist.el

(defvar eassist-header-switches '(("h" "cpp" "cc" "c") ("hpp" "cpp" "cc") ("cpp" "h" "hpp") ("c" "h") ("C" "H") ("H" "C" "CPP" "CC") ("cc" "h" "hpp")) "\
This variable defines possible switches for `eassist-switch-h-cpp' function.
Its format is list of (from . (to1 to2 to3...)) elements.  From and toN are
strings which are extentions of the files.")

(autoload 'eassist-switch-h-cpp "eassist" "\
Switch header and body file according to `eassist-header-switches' var.
The current buffer's file name extention is searched in
`eassist-header-switches' variable to find out extention for file's counterpart,
for example *.hpp <--> *.cpp.

\(fn)" t nil)

(autoload 'eassist-list-methods "eassist" "\
Show method/function list of current buffer in a newly created buffer.
This function is recommended to be bound to some convinient hotkey.

\(fn)" t nil)

;;;***

;;;### (autoloads (ede-target-parent ede-parent-project ede-load-project-file
;;;;;;  project-make-dist project-compile-target project-compile-project
;;;;;;  project-edit-file-target ede-compile-target ede-remove-file
;;;;;;  global-ede-mode) "ede" "cedet/ede/ede.el" (19539 30850))
;;; Generated autoloads from cedet/ede/ede.el

(defvar ede-projects nil "\
A list of all active projects currently loaded in Emacs.")

(defvar ede-minor-mode nil "\
Non-nil in EDE controlled buffers.")

(autoload 'global-ede-mode "ede" "\
Turn on variable `ede-minor-mode' mode when ARG is positive.
If ARG is negative, disable.  Toggle otherwise.

\(fn ARG)" t nil)

(autoload 'ede-remove-file "ede" "\
Remove the current file from targets.
Optional argument FORCE forces the file to be removed without asking.

\(fn &optional FORCE)" t nil)

(autoload 'ede-compile-target "ede" "\
Compile the current buffer's associated target.

\(fn)" t nil)

(autoload 'project-edit-file-target "ede" "\
Edit the target OT associated w/ this file.

\(fn (OT ede-target))" nil nil)

(autoload 'project-compile-project "ede" "\
Compile the entire current project OBJ.
Argument COMMAND is the command to use when compiling.

\(fn (OBJ ede-project) &optional COMMAND)" nil nil)

(autoload 'project-compile-target "ede" "\
Compile the current target OBJ.
Argument COMMAND is the command to use for compiling the target.

\(fn (OBJ ede-target) &optional COMMAND)" nil nil)

(autoload 'project-make-dist "ede" "\
Build a distribution for the project based on THIS project.

\(fn (THIS ede-project))" nil nil)

(autoload 'ede-load-project-file "ede" "\
Project file independent way to read a project in from DIR.
Optional ROOTRETURN will return the root project for DIR.

\(fn DIR &optional ROOTRETURN)" nil nil)

(autoload 'ede-parent-project "ede" "\
Return the project belonging to the parent directory.
Return nil if there is no previous directory.
Optional argument OBJ is an object to find the parent of.

\(fn &optional OBJ)" nil nil)

(autoload 'ede-target-parent "ede" "\
Return the project which is the parent of TARGET.
It is recommended you track the project a different way as this function
could become slow in time.

\(fn TARGET)" nil nil)

;;;***

;;;### (autoloads nil "ede-auto" "cedet/ede/ede-auto.el" (19358 14566))
;;; Generated autoloads from cedet/ede/ede-auto.el

(eieio-defclass-autoload 'ede-project-autoload 'nil "ede-auto" "Class representing minimal knowledge set to run preliminary EDE functions.\nWhen more advanced functionality is needed from a project type, that projects\ntype is required and the load function used.")

;;;***

;;;### (autoloads (ede-adebug-project-root ede-adebug-project-parent
;;;;;;  ede-adebug-project ede-documentation-files ede-description
;;;;;;  ede-name) "ede-base" "cedet/ede/ede-base.el" (19539 30884))
;;; Generated autoloads from cedet/ede/ede-base.el

(eieio-defclass-autoload 'ede-target '(eieio-speedbar-directory-button) "ede-base" "A target is a structure that describes a file set that produces something.\nTargets, as with 'Make', is an entity that will manage a file set \nand knows how to compile or otherwise transform those files into some\nother desired outcome.")

(eieio-defclass-autoload 'ede-project '(ede-project-placeholder) "ede-base" "Top level EDE project specification.\nAll specific project types must derive from this project.")

(autoload 'ede-name "ede-base" "\
Return the name of THIS target.

\(fn (THIS ede-target))" nil nil)

(autoload 'ede-description "ede-base" "\
Return a description suitable for the minibuffer about THIS.

\(fn (THIS ede-project))" nil nil)

(autoload 'ede-documentation-files "ede-base" "\
Return the documentation files for the current buffer.
Not all buffers need documentations, so return nil if no applicable.
Some projects may have multiple documentation files, so return a list.

\(fn)" nil nil)

(autoload 'ede-adebug-project "ede-base" "\
Run adebug against the current EDE project.
Display the results as a debug list.

\(fn)" t nil)

(autoload 'ede-adebug-project-parent "ede-base" "\
Run adebug against the current EDE parent project.
Display the results as a debug list.

\(fn)" t nil)

(autoload 'ede-adebug-project-root "ede-base" "\
Run adebug against the current EDE parent project.
Display the results as a debug list.

\(fn)" t nil)

;;;***

;;;### (autoloads (ede-cpp-root-load ede-cpp-root-project-root ede-cpp-root-project-file-for-dir)
;;;;;;  "ede-cpp-root" "cedet/ede/ede-cpp-root.el" (19565 48709))
;;; Generated autoloads from cedet/ede/ede-cpp-root.el

(autoload 'ede-cpp-root-project-file-for-dir "ede-cpp-root" "\
Return a full file name to the project file stored in DIR.

\(fn &optional DIR)" nil nil)

(autoload 'ede-cpp-root-project-root "ede-cpp-root" "\
Get the root directory for DIR.

\(fn &optional DIR)" nil nil)

(autoload 'ede-cpp-root-load "ede-cpp-root" "\
Return a CPP root object if you created one.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project.

\(fn DIR &optional ROOTPROJ)" nil nil)

(add-to-list 'ede-project-class-files (ede-project-autoload "cpp-root" :name "CPP ROOT" :file 'ede-cpp-root :proj-file 'ede-cpp-root-project-file-for-dir :proj-root 'ede-cpp-root-project-root :load-type 'ede-cpp-root-load :class-sym 'ede-cpp-root :new-p nil) t)

(eieio-defclass-autoload 'ede-cpp-root-project '(ede-project eieio-instance-tracker) "ede-cpp-root" "EDE cpp-root project class.\nEach directory needs a project file to control it.")

;;;***

;;;### (autoloads (ede-project-sort-targets ede-customize-target
;;;;;;  ede-customize-current-target ede-customize-project) "ede-custom"
;;;;;;  "cedet/ede/ede-custom.el" (19441 57882))
;;; Generated autoloads from cedet/ede/ede-custom.el

(autoload 'ede-customize-project "ede-custom" "\
Edit fields of the current project through EIEIO & Custom.

\(fn)" t nil)

(defalias 'customize-project 'ede-customize-project)

(autoload 'ede-customize-current-target "ede-custom" "\
Edit fields of the current target through EIEIO & Custom.
Optional argument OBJ is the target object to customize.

\(fn)" t nil)

(defalias 'customize-target 'ede-customize-current-target)

(autoload 'ede-customize-target "ede-custom" "\
Edit fields of the current target through EIEIO & Custom.
Optional argument OBJ is the target object to customize.

\(fn OBJ)" nil nil)

(autoload 'ede-project-sort-targets "ede-custom" "\
Create a custom-like buffer for sorting targets of current project.

\(fn)" t nil)

;;;***

;;;### (autoloads (ede-emacs-load ede-emacs-project-root) "ede-emacs"
;;;;;;  "cedet/ede/ede-emacs.el" (19441 57958))
;;; Generated autoloads from cedet/ede/ede-emacs.el

(autoload 'ede-emacs-project-root "ede-emacs" "\
Get the root directory for DIR.

\(fn &optional DIR)" nil nil)

(autoload 'ede-emacs-load "ede-emacs" "\
Return an Emacs Project object if there is a match.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project.

\(fn DIR &optional ROOTPROJ)" nil nil)

(add-to-list 'ede-project-class-files (ede-project-autoload "emacs" :name "EMACS ROOT" :file 'ede-emacs :proj-file "src/emacs.c" :proj-root 'ede-emacs-project-root :load-type 'ede-emacs-load :class-sym 'ede-emacs-project :new-p nil) t)

(eieio-defclass-autoload 'ede-emacs-project '(ede-project eieio-instance-tracker) "ede-emacs" "Project Type for the Emacs source code.")

;;;***

;;;### (autoloads (ede-find-file) "ede-files" "cedet/ede/ede-files.el"
;;;;;;  (19560 7790))
;;; Generated autoloads from cedet/ede/ede-files.el

(autoload 'ede-find-file "ede-files" "\
Find FILE in project.  FILE can be specified without a directory.
There is no completion at the prompt.  FILE is searched for within
the current EDE project.

\(fn FILE)" t nil)

;;;***

;;;### (autoloads (ede-enable-generic-projects ede-generic-load)
;;;;;;  "ede-generic" "cedet/ede/ede-generic.el" (19532 17979))
;;; Generated autoloads from cedet/ede/ede-generic.el

(autoload 'ede-generic-load "ede-generic" "\
Return a Generic Project object if there is a match.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project.

\(fn DIR &optional ROOTPROJ)" nil nil)

(autoload 'ede-enable-generic-projects "ede-generic" "\
Enable generic project loaders.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "ede-gnustep" "cedet/contrib/ede-gnustep.el"
;;;;;;  (19474 55392))
;;; Generated autoloads from cedet/contrib/ede-gnustep.el

(add-to-list 'ede-project-class-files (ede-project-autoload "edegnustep" :name "GNUstep-Make" :file 'ede-gnustep :proj-file "ProjStep.ede" :load-type 'ede-step-load :class-sym 'ede-step-project) t)

(add-to-list 'ede-project-class-files (ede-project-autoload "gnustep-root" :name "GNUstep-make Top Most" :file 'ede-gnustep :proj-file "RootProjStep.ede" :initializers '(:project-mode scanner) :load-type 'ede-gnustep-load :class-sym 'ede-step-project) t)

(add-to-list 'ede-project-class-files (ede-project-autoload "gnustep" :name "GNUstep-Make in scanner mode" :file 'ede-gnustep :proj-file "ProjStep.ede" :initializers '(:project-mode scanner) :load-type 'ede-gnustep-load :class-sym 'ede-step-project) t)

(add-to-list 'auto-mode-alist '("\\(Root\\)?ProjStep\\.ede" . emacs-lisp-mode))

;;;***

;;;### (autoloads (ede-linux-load ede-linux-project-root) "ede-linux"
;;;;;;  "cedet/ede/ede-linux.el" (19441 57971))
;;; Generated autoloads from cedet/ede/ede-linux.el

(autoload 'ede-linux-project-root "ede-linux" "\
Get the root directory for DIR.

\(fn &optional DIR)" nil nil)

(autoload 'ede-linux-load "ede-linux" "\
Return an Linux Project object if there is a match.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project.

\(fn DIR &optional ROOTPROJ)" nil nil)

(add-to-list 'ede-project-class-files (ede-project-autoload "linux" :name "LINUX ROOT" :file 'ede-linux :proj-file "scripts/ver_linux" :proj-root 'ede-linux-project-root :load-type 'ede-linux-load :class-sym 'ede-linux-project :new-p nil) t)

(eieio-defclass-autoload 'ede-linux-project '(ede-project eieio-instance-tracker) "ede-linux" "Project Type for the Linux source code.")

;;;***

;;;### (autoloads (ede-enable-locate-on-project) "ede-locate" "cedet/ede/ede-locate.el"
;;;;;;  (19560 7649))
;;; Generated autoloads from cedet/ede/ede-locate.el

(autoload 'ede-enable-locate-on-project "ede-locate" "\
Enable an EDE locate feature on PROJECT.
Attempt to guess which project locate style to use
based on `ede-locate-setup-options'.

\(fn &optional PROJECT)" t nil)

;;;***

;;;### (autoloads (ede-make-check-version) "ede-make" "cedet/ede/ede-make.el"
;;;;;;  (19390 33968))
;;; Generated autoloads from cedet/ede/ede-make.el

(autoload 'ede-make-check-version "ede-make" "\
Check the version of GNU Make installed.
The check passes if the MAKE version is no high enough, or if it
is not GNU make.
If NOERROR is non-nil, return t for success, nil for failure.
If NOERROR is nil, then throw an error on failure.  Return t otherwise.

\(fn &optional NOERROR)" t nil)

;;;***

;;;### (autoloads (ede-pmake-varname) "ede-pmake" "cedet/ede/ede-pmake.el"
;;;;;;  (19358 14566))
;;; Generated autoloads from cedet/ede/ede-pmake.el

(autoload 'ede-pmake-varname "ede-pmake" "\
Convert OBJ into a variable name name.
Change .  to _ in the variable name.

\(fn OBJ)" nil nil)

;;;***

;;;### (autoloads nil "ede-proj" "cedet/ede/ede-proj.el" (19467 44840))
;;; Generated autoloads from cedet/ede/ede-proj.el

(add-to-list 'auto-mode-alist '("Project\\.ede$" . emacs-lisp-mode))

;;;***

;;;### (autoloads (ede-shell-buffer ede-shell-run-something) "ede-shell"
;;;;;;  "cedet/ede/ede-shell.el" (19358 14566))
;;; Generated autoloads from cedet/ede/ede-shell.el

(autoload 'ede-shell-run-something "ede-shell" "\
Create a shell to run stuff for TARGET.
COMMAND is a text string representing the thing to be run.

\(fn (TARGET ede-target) COMMAND)" nil nil)

(autoload 'ede-shell-buffer "ede-shell" "\
Get the buffer for running shell commands for TARGET.

\(fn (TARGET ede-target))" nil nil)

;;;***

;;;### (autoloads (ede-simple-load ede-simple-projectfile-for-dir)
;;;;;;  "ede-simple" "cedet/ede/ede-simple.el" (19532 13882))
;;; Generated autoloads from cedet/ede/ede-simple.el

(autoload 'ede-simple-projectfile-for-dir "ede-simple" "\
Return a full file name to the project file stored in the current directory.
The directory has three parts:
  <STORAGE ROOT>/<PROJ DIR AS FILE>/ProjSimple.ede

\(fn &optional DIR)" nil nil)

(autoload 'ede-simple-load "ede-simple" "\
Load a project of type `Simple' for the directory DIR.
Return nil if there isn't one.
ROOTPROJ is nil, since we will only create a single EDE project here.

\(fn DIR &optional ROOTPROJ)" nil nil)

(eieio-defclass-autoload 'ede-simple-project '(ede-project eieio-persistent) "ede-simple" "EDE Simple project class.\nEach directory needs a project file to control it.")

;;;***

;;;### (autoloads (ede-srecode-insert ede-srecode-setup) "ede-srecode"
;;;;;;  "cedet/ede/ede-srecode.el" (19530 61596))
;;; Generated autoloads from cedet/ede/ede-srecode.el

(autoload 'ede-srecode-setup "ede-srecode" "\
Update various paths to get SRecode to identify our macros.

\(fn)" nil nil)

(autoload 'ede-srecode-insert "ede-srecode" "\
Insert at the current point TEMPLATE.
TEMPLATE should specify a context by using a string format of:
  context:templatename
Add DICTIONARY-ENTRIES into the dictionary before insertion.
Note: Just like `srecode-insert', but templates found in 'ede app.

\(fn TEMPLATE &rest DICTIONARY-ENTRIES)" nil nil)

;;;***

;;;### (autoloads (ede-update-version) "ede-util" "cedet/ede/ede-util.el"
;;;;;;  (19358 14566))
;;; Generated autoloads from cedet/ede/ede-util.el

(autoload 'ede-update-version "ede-util" "\
Update the current projects main version number.
Argument NEWVERSION is the version number to use in the current project.

\(fn NEWVERSION)" t nil)

;;;***

;;;### (autoloads (data-debug-show data-debug-insert-object-button
;;;;;;  data-debug-insert-object-slots) "eieio-datadebug" "cedet/eieio/eieio-datadebug.el"
;;;;;;  (19115 2171))
;;; Generated autoloads from cedet/eieio/eieio-datadebug.el

(autoload 'data-debug-insert-object-slots "eieio-datadebug" "\
Insert all the slots of OBJECT.
PREFIX specifies what to insert at the start of each line.

\(fn OBJECT PREFIX)" nil nil)

(autoload 'data-debug-insert-object-button "eieio-datadebug" "\
Insert a button representing OBJECT.
PREFIX is the text that preceeds the button.
PREBUTTONTEXT is some text between PREFIX and the object button.

\(fn OBJECT PREFIX PREBUTTONTEXT)" nil nil)

(autoload 'data-debug-show "eieio-datadebug" "\
Run ddebug against any EIEIO object OBJ

\(fn (OBJ eieio-default-superclass))" nil nil)

;;;***

;;;### (autoloads (eieio-describe-generic eieio-build-class-alist
;;;;;;  eieio-describe-constructor eieio-describe-class eieio-browse)
;;;;;;  "eieio-opt" "cedet/eieio/eieio-opt.el" (19390 36062))
;;; Generated autoloads from cedet/eieio/eieio-opt.el

(autoload 'eieio-browse "eieio-opt" "\
Create an object browser window to show all objects.
If optional ROOT-CLASS, then start with that, otherwise start with
variable `eieio-default-superclass'.

\(fn &optional ROOT-CLASS)" t nil)

(defalias 'describe-class 'eieio-describe-class)

(autoload 'eieio-describe-class "eieio-opt" "\
Describe a CLASS defined by a string or symbol.
If CLASS is actually an object, then also display current values of that obect.
Optional HEADERFCN should be called to insert a few bits of info first.

\(fn CLASS &optional HEADERFCN)" t nil)

(autoload 'eieio-describe-constructor "eieio-opt" "\
Describe the constructor function FCN.
Uses `eieio-describe-class' to describe the class being constructed.

\(fn FCN)" t nil)

(autoload 'eieio-build-class-alist "eieio-opt" "\
Return an alist of all currently active classes for completion purposes.
Optional argument CLASS is the class to start with.
If INSTANTIABLE-ONLY is non nil, only allow names of classes which
are not abstract, otherwise allow all classes.
Optional argument BUILDLIST is more list to attach and is used internally.

\(fn &optional CLASS INSTANTIABLE-ONLY BUILDLIST)" nil nil)

(defalias 'describe-method 'eieio-describe-generic)

(defalias 'describe-generic 'eieio-describe-generic)

(defalias 'eieio-describe-method 'eieio-describe-generic)

(autoload 'eieio-describe-generic "eieio-opt" "\
Describe the generic function GENERIC.
Also extracts information about all methods specific to this generic.

\(fn GENERIC)" t nil)

;;;***

;;;### (autoloads (eieio-perftest-onemethodcall eieio-perftest-methodcall)
;;;;;;  "eieio-perftest" "cedet/eieio/eieio-perftest.el" (19010 46106))
;;; Generated autoloads from cedet/eieio/eieio-perftest.el

(autoload 'eieio-perftest-methodcall "eieio-perftest" "\
Test and time performance of method invocation.

\(fn)" t nil)

(autoload 'eieio-perftest-onemethodcall "eieio-perftest" "\
Test and time performance of method invocation.

\(fn)" t nil)

;;;***

;;;### (autoloads (object-write-xml) "eieio-xml" "cedet/eieio/eieio-xml.el"
;;;;;;  (18656 8049))
;;; Generated autoloads from cedet/eieio/eieio-xml.el

(autoload 'object-write-xml "eieio-xml" "\
Write object THIS out to the current stream as XML.
  If optional COMMENT is non-nil, include comments when outputting
this object.
@todo - support arbitrary schema output

\(fn (THIS eieio-default-superclass) &optional COMMENT)" nil nil)

;;;***

;;;### (autoloads (emms-cache-toggle emms-cache-disable emms-cache-enable)
;;;;;;  "emms-cache" "emms/lisp/emms-cache.el" (19793 16447))
;;; Generated autoloads from emms/lisp/emms-cache.el

(autoload 'emms-cache-enable "emms-cache" "\
Enable caching of Emms track data.

\(fn)" t nil)

(autoload 'emms-cache-disable "emms-cache" "\
Disable caching of Emms track data.

\(fn)" t nil)

(autoload 'emms-cache-toggle "emms-cache" "\
Toggle caching of Emms track data.

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-lyrics-toggle emms-lyrics-disable emms-lyrics-enable)
;;;;;;  "emms-lyrics" "emms/lisp/emms-lyrics.el" (19793 16447))
;;; Generated autoloads from emms/lisp/emms-lyrics.el

(autoload 'emms-lyrics-enable "emms-lyrics" "\
Enable displaying emms lyrics.

\(fn)" t nil)

(autoload 'emms-lyrics-disable "emms-lyrics" "\
Disable displaying emms lyrics.

\(fn)" t nil)

(autoload 'emms-lyrics-toggle "emms-lyrics" "\
Toggle displaying emms lyrics.

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-mode-line-toggle emms-mode-line-disable emms-mode-line-enable)
;;;;;;  "emms-mode-line" "emms/lisp/emms-mode-line.el" (19793 16447))
;;; Generated autoloads from emms/lisp/emms-mode-line.el

(autoload 'emms-mode-line-enable "emms-mode-line" "\
Turn on `emms-mode-line'.

\(fn)" t nil)

(autoload 'emms-mode-line-disable "emms-mode-line" "\
Turn off `emms-mode-line'.

\(fn)" t nil)

(autoload 'emms-mode-line-toggle "emms-mode-line" "\
Toggle `emms-mode-line'.

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-player-mpd-show emms-player-mpd-connect emms-player-mpd-clear)
;;;;;;  "emms-player-mpd" "emms/lisp/emms-player-mpd.el" (19793 16447))
;;; Generated autoloads from emms/lisp/emms-player-mpd.el

(autoload 'emms-player-mpd-clear "emms-player-mpd" "\
Clear the MusicPD playlist.

\(fn)" t nil)

(autoload 'emms-player-mpd-connect "emms-player-mpd" "\
Connect to MusicPD and retrieve its current playlist.

Afterward, the status of MusicPD will be tracked.

This also has the effect of changing the current EMMS playlist to
be the same as the current MusicPD playlist.  Thus, this
function is useful to call if the contents of the EMMS playlist
buffer get out-of-sync for some reason.

\(fn)" t nil)

(autoload 'emms-player-mpd-show "emms-player-mpd" "\
Describe the current EMMS track in the minibuffer.

If INSERTP is non-nil, insert the description into the current
buffer instead.

If CALLBACK is a function, call it with the current buffer and
description as arguments instead of displaying the description or
inserting it.

This function uses `emms-show-format' to format the current track.
It differs from `emms-show' in that it asks MusicPD for the current track,
rather than EMMS.

\(fn &optional INSERTP CALLBACK)" t nil)

;;;***

;;;### (autoloads (emms-playing-time-disable-display emms-playing-time-enable-display)
;;;;;;  "emms-playing-time" "emms/lisp/emms-playing-time.el" (19793
;;;;;;  16447))
;;; Generated autoloads from emms/lisp/emms-playing-time.el

(autoload 'emms-playing-time-enable-display "emms-playing-time" "\
Display playing time on mode line.

\(fn)" t nil)

(autoload 'emms-playing-time-disable-display "emms-playing-time" "\
Remove playing time from mode line.

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-playlist-limit-toggle emms-playlist-limit-disable
;;;;;;  emms-playlist-limit-enable) "emms-playlist-limit" "emms/lisp/emms-playlist-limit.el"
;;;;;;  (19793 16447))
;;; Generated autoloads from emms/lisp/emms-playlist-limit.el

(autoload 'emms-playlist-limit-enable "emms-playlist-limit" "\
Turn on emms playlist limit.

\(fn)" t nil)

(autoload 'emms-playlist-limit-disable "emms-playlist-limit" "\
Turn off emms playlist limit.

\(fn)" t nil)

(autoload 'emms-playlist-limit-toggle "emms-playlist-limit" "\
Toggle emms playlist limit.

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-playlist-mode) "emms-playlist-mode" "emms/lisp/emms-playlist-mode.el"
;;;;;;  (19793 16447))
;;; Generated autoloads from emms/lisp/emms-playlist-mode.el

(autoload 'emms-playlist-mode "emms-playlist-mode" "\
A major mode for Emms playlists.
\\{emms-playlist-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-score-toggle emms-score-disable emms-score-enable)
;;;;;;  "emms-score" "emms/lisp/emms-score.el" (19793 16447))
;;; Generated autoloads from emms/lisp/emms-score.el

(autoload 'emms-score-enable "emms-score" "\
Turn on emms-score.

\(fn)" t nil)

(autoload 'emms-score-disable "emms-score" "\
Turn off emms-score.

\(fn)" t nil)

(autoload 'emms-score-toggle "emms-score" "\
Toggle emms-score.

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-default-players emms-devel emms-all emms-standard
;;;;;;  emms-minimalistic) "emms-setup" "emms/lisp/emms-setup.el"
;;;;;;  (19793 16447))
;;; Generated autoloads from emms/lisp/emms-setup.el

(autoload 'emms-minimalistic "emms-setup" "\
An Emms setup script.
Invisible playlists and all the basics for playing media.

\(fn)" nil nil)

(autoload 'emms-standard "emms-setup" "\
An Emms setup script.
Everything included in the `emms-minimalistic' setup, the Emms
interactive playlist mode, reading information from tagged
audio files, and a metadata cache.

\(fn)" nil nil)

(autoload 'emms-all "emms-setup" "\
An Emms setup script.
Everything included in the `emms-standard' setup and adds all the
stable features which come with the Emms distribution.

\(fn)" nil nil)

(autoload 'emms-devel "emms-setup" "\
An Emms setup script.
Everything included in the `emms-all' setup and adds all the
features which come with the Emms distribution regardless of if
they are considered stable or not.  Use this if you like living
on the edge.

\(fn)" nil nil)

(autoload 'emms-default-players "emms-setup" "\
Set `emms-player-list' to `emms-setup-default-player-list'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (emms-locate emms-source-file-regex emms-source-file-directory-tree)
;;;;;;  "emms-source-file" "emms/lisp/emms-source-file.el" (19793
;;;;;;  16447))
;;; Generated autoloads from emms/lisp/emms-source-file.el
 (autoload 'emms-play-file "emms-source-file" nil t)
 (autoload 'emms-add-file "emms-source-file" nil t)
 (autoload 'emms-play-directory "emms-source-file" nil t)
 (autoload 'emms-add-directory "emms-source-file" nil t)
 (autoload 'emms-play-directory-tree "emms-source-file" nil t)
 (autoload 'emms-add-directory-tree "emms-source-file" nil t)
 (autoload 'emms-play-find "emms-source-file" nil t)
 (autoload 'emms-add-find "emms-source-file" nil t)
 (autoload 'emms-play-dired "emms-source-file" nil t)
 (autoload 'emms-add-dired "emms-source-file" nil t)

(autoload 'emms-source-file-directory-tree "emms-source-file" "\
Return a list of all files under DIR that match REGEX.
This function uses `emms-source-file-directory-tree-function'.

\(fn DIR REGEX)" nil nil)

(autoload 'emms-source-file-regex "emms-source-file" "\
Return a regexp that matches everything any player (that supports
files) can play.

\(fn)" nil nil)

(autoload 'emms-locate "emms-source-file" "\
Search for REGEXP and display the results in a locate buffer

\(fn REGEXP)" t nil)
 (autoload 'emms-play-url "emms-source-file" nil t)
 (autoload 'emms-add-url "emms-source-file" nil t)
 (autoload 'emms-play-streamlist "emms-source-file" nil t)
 (autoload 'emms-add-streamlist "emms-source-file" nil t)
 (autoload 'emms-play-lastfm "emms-lastfm" nil t)
 (autoload 'emms-add-lastfm "emms-lastfm" nil t)

;;;***

;;;### (autoloads nil "emms-source-playlist" "emms/lisp/emms-source-playlist.el"
;;;;;;  (19793 16447))
;;; Generated autoloads from emms/lisp/emms-source-playlist.el
 (autoload 'emms-play-playlist "emms-source-playlist" nil t)
 (autoload 'emms-add-playlist "emms-source-playlist" nil t)
 (autoload 'emms-play-native-playlist "emms-source-playlist" nil t)
 (autoload 'emms-add-native-playlist "emms-source-playlist" nil t)
 (autoload 'emms-play-m3u-playlist "emms-source-playlist" nil t)
 (autoload 'emms-add-m3u-playlist "emms-source-playlist" nil t)
 (autoload 'emms-play-pls-playlist "emms-source-playlist" nil t)
 (autoload 'emms-add-pls-playlist "emms-source-playlist" nil t)
 (autoload 'emms-play-playlist-file "emms-source-playlist" nil t)
 (autoload 'emms-add-playlist-file "emms-source-playlist" nil t)
 (autoload 'emms-play-playlist-directory
          "emms-source-playlist" nil t)
 (autoload 'emms-add-playlist-directory
          "emms-source-playlist" nil t)
 (autoload 'emms-play-playlist-directory-tree
          "emms-source-playlist" nil t)
 (autoload 'emms-add-playlist-directory-tree
          "emms-source-file" nil t)

;;;***

;;;### (autoloads (emms-streams) "emms-streams" "emms/lisp/emms-streams.el"
;;;;;;  (19793 16447))
;;; Generated autoloads from emms/lisp/emms-streams.el

(autoload 'emms-streams "emms-streams" "\
Opens the EMMS Streams interface.

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-volume-mode-minus emms-volume-mode-plus emms-volume-lower
;;;;;;  emms-volume-raise) "emms-volume" "emms/lisp/emms-volume.el"
;;;;;;  (19793 16447))
;;; Generated autoloads from emms/lisp/emms-volume.el

(autoload 'emms-volume-raise "emms-volume" "\
Raise the speaker volume.

\(fn)" t nil)

(autoload 'emms-volume-lower "emms-volume" "\
Lower the speaker volume.

\(fn)" t nil)

(autoload 'emms-volume-mode-plus "emms-volume" "\
Raise volume and enable or extend the `emms-volume-minor-mode' timeout.

\(fn)" t nil)

(autoload 'emms-volume-mode-minus "emms-volume" "\
Lower volume and enable or extend the `emms-volume-minor-mode' timeout.

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-volume-amixer-change) "emms-volume-amixer"
;;;;;;  "emms/lisp/emms-volume-amixer.el" (19793 16447))
;;; Generated autoloads from emms/lisp/emms-volume-amixer.el

(autoload 'emms-volume-amixer-change "emms-volume-amixer" "\
Change amixer master volume by AMOUNT.

\(fn AMOUNT)" nil nil)

;;;***

;;;### (autoloads (espresso-mode) "espresso" "espresso.el" (19823
;;;;;;  60498))
;;; Generated autoloads from espresso.el

(autoload 'espresso-mode "espresso" "\
Major mode for editing JavaScript source text.

Key bindings:

\\{espresso-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (with-expect) "expect" "mailcrypt-3.5.9/expect.el"
;;;;;;  (19352 62013))
;;; Generated autoloads from mailcrypt-3.5.9/expect.el

(autoload 'with-expect "expect" "\
Set things up for communication with PROGRAM.
FORMS will be evaluated in the normal manner.  To talk to the process,
use `expect' and `expect-send'.  See the manual for full documentation.
This macro returns nil.

If PROGRAM is a string, start that program.  If PROGRAM is a list, use
the first element of that list as the program and the remainder as the
parameters.  If PROGRAM is a process, talk to that process.

PROGRAM will be started up in a new, fresh temporary buffer.  The
buffer will be killed upon completion.  If PROGRAM is a process,
a new buffer won't be created, and the buffer won't be killed upon
completion.

\(fn PROGRAM &rest FORMS)" nil (quote macro))

;;;***

;;;### (autoloads (define-fame-channel) "fame" "cedet/common/fame.el"
;;;;;;  (17213 39681))
;;; Generated autoloads from cedet/common/fame.el

(autoload 'define-fame-channel "fame" "\
Define the new message channel CHANNEL.
CHANNEL must be a non-nil symbol.
The optional argument DEFAULT specifies the default value of message
levels for this channel.  By default it is the value of
`fame-default-level-values'.
DOCSTRING is an optional channel documentation.

This defines the option `CHANNEL-fame-levels' to customize the current
value of message levels.  And the functions `CHANNEL-send-debug',
`CHANNEL-send-info', `CHANNEL-send-warning', and `CHANNEL-send-error',
that respectively send debug, informational, warning, and error
messages to CHANNEL.

\(fn CHANNEL &optional DEFAULT DOCSTRING)" nil (quote macro))

;;;***

;;;### (autoloads (fit-frame-or-mouse-drag-vertical-line fit-frame-to-image
;;;;;;  fit-frame fit-frame-skip-header-lines-alist fit-frame-fill-column-margin
;;;;;;  fit-frame-empty-special-display-height fit-frame-empty-special-display-width
;;;;;;  fit-frame-empty-height fit-frame-empty-width fit-frame-max-height-percent
;;;;;;  fit-frame-max-height fit-frame-min-height fit-frame-max-width-percent
;;;;;;  fit-frame-max-width fit-frame-min-width fit-frame-crop-end-blank-flag
;;;;;;  fit-frame-inhibit-fitting-flag fit-frame) "fit-frame" "fit-frame.el"
;;;;;;  (19816 5505))
;;; Generated autoloads from fit-frame.el

(let ((loads (get 'fit-frame 'custom-loads))) (if (member '"fit-frame" loads) nil (put 'fit-frame 'custom-loads (cons '"fit-frame" loads))))

(defvar fit-frame-inhibit-fitting-flag nil "\
*Non-nil means command `fit-frame' does nothing.
You can bind this to non-`nil' to temporarily inhibit frame fitting:
    (let ((fit-frame-inhibit-fitting-flag  t))...)")

(custom-autoload 'fit-frame-inhibit-fitting-flag "fit-frame" t)

(defvar fit-frame-crop-end-blank-flag nil "\
*Non-nil means `fit-frame' doesn't count blank lines at end of buffer.
If nil, then fitting leaves room for such blank lines.")

(custom-autoload 'fit-frame-crop-end-blank-flag "fit-frame" t)

(defvar fit-frame-min-width window-min-width "\
*Minimum width, in characters, that `fit-frame' gives to a frame.
The actual minimum is at least the greater of this and `window-min-width'.")

(custom-autoload 'fit-frame-min-width "fit-frame" t)

(defvar fit-frame-max-width nil "\
*Maximum width, in characters, that `fit-frame' gives to a frame.
If nil, then the function `fit-frame-max-width' is used instead.")

(custom-autoload 'fit-frame-max-width "fit-frame" t)

(defvar fit-frame-max-width-percent 94 "\
*Maximum percent of display width that `fit-frame' gives to a frame'.
See function `fit-frame-max-width'.
Not used unless `fit-frame-max-width' is nil.")

(custom-autoload 'fit-frame-max-width-percent "fit-frame" t)

(defvar fit-frame-min-height window-min-height "\
*Minimum height, in lines, that `fit-frame' gives to a frame.
The actual minimum is at least the greater of this and `window-min-height'.")

(custom-autoload 'fit-frame-min-height "fit-frame" t)

(defvar fit-frame-max-height nil "\
*Maximum height, in lines, that `fit-frame' gives to a frame.
If nil, then the function `fit-frame-max-height' is used instead.")

(custom-autoload 'fit-frame-max-height "fit-frame" t)

(defvar fit-frame-max-height-percent 82 "\
*Maximum percent of display height that `fit-frame' gives to a frame.
See function `fit-frame-max-height'.
Not used unless `fit-frame-max-height' is nil.")

(custom-autoload 'fit-frame-max-height-percent "fit-frame" t)

(defvar fit-frame-empty-width (or (cdr (assq 'width default-frame-alist)) 80) "\
*Width, in characters, that `fit-frame' gives to an empty frame.")

(custom-autoload 'fit-frame-empty-width "fit-frame" t)

(defvar fit-frame-empty-height (or (cdr (assq 'height default-frame-alist)) 35) "\
*Height, in lines, that `fit-frame' gives to an empty frame.")

(custom-autoload 'fit-frame-empty-height "fit-frame" t)

(defvar fit-frame-empty-special-display-width 80 "\
*Width, in chars, that `fit-frame' gives to an empty special-display frame.
If this is nil, it is ignored.")

(custom-autoload 'fit-frame-empty-special-display-width "fit-frame" t)

(defvar fit-frame-empty-special-display-height 9 "\
*Height, in lines, that `fit-frame' gives to an empty special-display frame.
If this is nil, it is ignored.")

(custom-autoload 'fit-frame-empty-special-display-height "fit-frame" t)

(defvar fit-frame-fill-column-margin 7 "\
*Difference between `fill-column' and frame width after fitting a frame.
Used when `fit-frame' fits a frame, if the prefix arg is negative.
Depending on the average word length of the language used in the
selected window, you might want different values for this.  This
variable is buffer-local.")

(custom-autoload 'fit-frame-fill-column-margin "fit-frame" t)

(defvar fit-frame-skip-header-lines-alist '((Info-mode . 1) (dired-mode . 2) (compilation-mode . 2)) "\
*Alist of major-modes and header lines to ignore.

When `fit-frame' calculates the width of the current buffer, it can
first skip some lines at the buffer beginning, ignoring their
widths.  For example, Info, Dired, and compilation buffers sometimes
have a long header line at the top.  You can use this alist to tell
`fit-frame' to ignore the width of these header lines.

Each item in the alist is of form (MODE . LINES).
 MODE is a major-mode name.
 LINES is the number of lines to skip at the beginning of the buffer.")

(custom-autoload 'fit-frame-skip-header-lines-alist "fit-frame" t)

(autoload 'fit-frame "fit-frame" "\
Resize FRAME to fit its buffer(s).
Does nothing if `fit-frame-inhibit-fitting-flag' is non-nil.

FRAME defaults to the current (i.e. selected) frame.

If non-nil, WIDTH and HEIGHT specify the frame width and height.  To
define them interactively, use a non-negative prefix arg (e.g. `C-9').

To set the width to `fill-column' + `fit-frame-fill-column-margin',
use a negative prefix arg (e.g. `M--').

To fit the frame to all of its displayed buffers, use no prefix arg.
To fit it to just the current buffer, use a plain prefix arg (`C-u').

Fitting a non-empty buffer means resizing the frame to the smallest
size such that the following are both true:

 * The width is at least `fit-frame-min-width' and `window-min-width'.
   The width is at most `fit-frame-max-width(-percent)' and the
   longest line length.

   (However, extra width is allowed for fringe, if shown.)

 * The height is at least `fit-frame-min-height' and
   `window-min-height'.  The height is at most
   `fit-frame-max-height(-percent)' and the number of lines.

You can thus use those user variables to control the maximum and
minimum frame sizes.  The `*-percent' options let you specify the
maximum as a percentage of your display size.

See also options `fit-frame-skip-header-lines-alist' and
`fit-frame-crop-end-blank-flag'.

The following user options control how an empty frame is fit.
An empty frame is a one-window frame displaying an empty buffer.

 * `fit-frame-empty-width', `fit-frame-empty-height' (normal buffer)
 * `fit-frame-empty-special-display-width',
   `fit-frame-empty-special-display-height' (special-display buffer)

Note: `fit-frame' does not take into account wrapping of a menu-bar
line.  There is no easy way to calculate the number of display lines
from such wrapping.

\(fn &optional FRAME WIDTH HEIGHT ALL-WINDOWS-P)" t nil)

(autoload 'fit-frame-to-image "fit-frame" "\
Fit FRAME to the current image.
If FRAME is not the selected frame, fit it to its first image.
Interactively, if frame has already been fit to the image, then
 restore the size from before it was fit.
This function assumes that FRAME has only one window.

\(fn INTERACTIVEP &optional FRAME)" t nil)

(autoload 'fit-frame-or-mouse-drag-vertical-line "fit-frame" "\
If only window in frame, `fit-frame'; else `mouse-drag-vertical-line'.

\(fn START-EVENT)" t nil)

;;;***

;;;### (autoloads (other-window-or-frame tell-customize-var-has-changed
;;;;;;  set-all-frame-alist-parameters-from-frame set-frame-alist-parameter-from-frame
;;;;;;  enlarge-font move-frame-left move-frame-right move-frame-up
;;;;;;  move-frame-down shrink-frame-horizontally shrink-frame enlarge-frame-horizontally
;;;;;;  enlarge-frame tile-frames-vertically tile-frames-horizontally
;;;;;;  toggle-max-frame toggle-max-frame-vertically toggle-max-frame-horizontally
;;;;;;  maximize-frame maximize-frame-vertically maximize-frame-horizontally
;;;;;;  delete-other-frames delete-1-window-frames-on show-*Help*-buffer
;;;;;;  show-a-frame-on hide-frame show-frame rename-non-minibuffer-frame
;;;;;;  rename-frame delete/iconify-windows-on delete/iconify-window
;;;;;;  mouse-remove-window remove-windows-on delete-windows-on delete-windows-for
;;;;;;  delete-window mouse-iconify/map-frame iconify/map-frame mouse-show-hide-mark-unmark
;;;;;;  show-buffer-menu show-hide hide-everything iconify-everything
;;;;;;  jump-to-frame-config-register save-frame-config) "frame-cmds"
;;;;;;  "frame-cmds.el" (19816 5574))
;;; Generated autoloads from frame-cmds.el

(autoload 'save-frame-config "frame-cmds" "\
Save current frame configuration.
You can restore it with \\[jump-to-frame-config-register].

\(fn)" t nil)

(autoload 'jump-to-frame-config-register "frame-cmds" "\
Restore frame configuration saved in `frame-config-register'.

\(fn)" t nil)

(autoload 'iconify-everything "frame-cmds" "\
Iconify all frames of session at once.
Remembers frame configuration in register `C-l' (Control-L).
To restore this frame configuration, use `\\[jump-to-register] C-l'.

\(fn)" t nil)

(autoload 'hide-everything "frame-cmds" "\
Hide all frames of session at once.
Iconify minibuffer frame; make all others invisible.
Remembers frame configuration in register `C-l' (Control-L).
To restore this frame configuration, use `\\[jump-to-register] C-l'.

\(fn)" t nil)

(autoload 'show-hide "frame-cmds" "\
1 frame visible: `show-hide-show-function'; else: `hide-everything'.
This acts as a toggle between showing all frames and showing only an
iconified minibuffer frame.

\(fn)" t nil)

(autoload 'show-buffer-menu "frame-cmds" "\
Call `buffer-menu' after making all frames visible.
Useful after using `hide-everything' because of a Windows bug that
doesn't let you display frames that have been made visible after
being made invisible.

\(fn)" t nil)

(autoload 'mouse-show-hide-mark-unmark "frame-cmds" "\
In minibuffer: `show-hide'.  In dired: mark/unmark; else: buffer menu.

\(fn EVENT)" t nil)

(autoload 'iconify/map-frame "frame-cmds" "\
Iconify selected frame if now mapped.  Map it if now iconified.
With non-nil prefix arg ICONIFY-ALL, iconify all visible frames.

\(fn &optional ICONIFY-ALL)" t nil)

(autoload 'mouse-iconify/map-frame "frame-cmds" "\
Iconify frame clicked on, if now mapped.  Map it if now iconified.

\(fn EVENT)" t nil)

(autoload 'delete-window "frame-cmds" "\
Remove WINDOW from the display.  Default is `selected-window'.
If WINDOW is the only one in its frame, then `delete-frame' too.

\(fn &optional WINDOW)" t nil)

(autoload 'delete-windows-for "frame-cmds" "\
`delete-window' or prompt for buffer and delete its windows.
With no prefix arg, delete the selected window.
With a prefix arg, prompt for a buffer and delete all windows, on any
  frame, that show that buffer.

\(fn &optional BUFFER)" t nil)

(autoload 'delete-windows-on "frame-cmds" "\
Delete windows showing BUFFER.
Optional arg BUFFER defaults to the current buffer.

Optional second arg FRAME controls which frames are considered.
  If nil or omitted, delete all windows showing BUFFER in any frame.
  If t, delete only windows showing BUFFER in the selected frame.
  If `visible', delete all windows showing BUFFER in any visible frame.
  If a frame, delete only windows showing BUFFER in that frame.

Interactively, FRAME depends on the prefix arg, as follows:
  Without a prefix arg (prefix = nil), FRAME is nil (all frames).
  With prefix arg >= 0, FRAME is t (this frame only).
  With prefix arg < 0,  FRAME is `visible' (all visible frames).

\(fn &optional BUFFER FRAME)" t nil)

(defalias 'remove-window 'delete-window)

(autoload 'remove-windows-on "frame-cmds" "\
Remove all windows showing BUFFER.  This calls `remove-window'
on each window showing BUFFER.

\(fn BUFFER)" t nil)

(autoload 'mouse-remove-window "frame-cmds" "\
Remove the window you click on.  (This calls `remove-window'.)
This command must be bound to a mouse click.

\(fn EVENT)" t nil)

(autoload 'delete/iconify-window "frame-cmds" "\
Delete or iconify WINDOW (default: `selected-window').
If WINDOW is the only one in its frame (`one-window-p'), then optional
arg FRAME-P determines the behavior regarding the frame, as follows:
  If FRAME-P is nil, then the frame is deleted (with the window).
  If FRAME-P is t, then the frame is iconified.
  If FRAME-P is a symbol naming a function, the function is applied
             to WINDOW as its only arg.
             If the result is nil, then the frame is deleted.
             If the result is non-nil, then the frame is iconified.
  If FRAME-P is anything else, then behavior is as if FRAME-P were the
             symbol `window-dedicated-p': the frame is iconified if
             WINDOW is dedicated, otherwise the frame is deleted.

Interactively, FRAME-P depends on the prefix arg, as follows:
  Without a prefix arg (prefix = nil), FRAME-P is `window-dedicated-p'.
  With prefix arg < 0, FRAME-P is t.  The frame is iconified.
  With prefix arg >= 0, FRAME-P is nil.  The frame is deleted.

\(fn &optional WINDOW FRAME-P)" t nil)

(autoload 'delete/iconify-windows-on "frame-cmds" "\
For each window showing BUFFER: delete it or iconify its frame.
\(This calls `delete/iconify-window' on each window showing BUFFER.)

Optional second arg FRAME controls which frames are considered.
  If nil or omitted, treat all windows showing BUFFER in any frame.
  If t, treat only windows showing BUFFER in the selected frame.
  If `visible', treat all windows showing BUFFER in any visible frame.
  If a frame, treat only windows showing BUFFER in that frame.

Optional third arg FRAME-P controls what to do with one-window frames.
  If FRAME-P is nil, then one-window frames showing BUFFER are deleted.
  If FRAME-P is t, then one-window frames are iconified.
  If FRAME-P is a symbol naming a function, the function is applied
             to each window showing buffer in a frame by itself.
             If the result is nil, then the frame is deleted.
             If the result is non-nil, then the frame is iconified.
  If FRAME-P is anything else, then behavior is as if FRAME-P were the
             symbol `window-dedicated-p': One-window frames are
             iconified if window is dedicated, else they are deleted.

Interactively, FRAME is nil, and FRAME-P depends on the prefix arg:
  Without a prefix arg (prefix = nil), FRAME-P is `window-dedicated-p'.
  With prefix arg < 0, FRAME-P is t.  The frame is iconified.
  With prefix arg >= 0, FRAME-P is nil.  The frame is deleted.

\(fn BUFFER &optional FRAME FRAME-P)" t nil)

(autoload 'rename-frame "frame-cmds" "\
Rename a frame named OLD-NAME to NEW-NAME.
Prefix arg ALL-NAMED non-nil means rename all frames named FRAME to NEWNAME.

OLD-NAME may be a frame, its name, or nil.  Default is `selected-frame'.

NEW-NAME is a string or nil.  Default NEW-NAME is current `buffer-name'.

\(fn &optional OLD-NAME NEW-NAME ALL-NAMED)" t nil)

(autoload 'rename-non-minibuffer-frame "frame-cmds" "\
Unless OLD-NAME names the minibuffer frame, use `rename-frame'
to rename a frame named OLD-NAME to NEW-NAME.

Prefix arg ALL-NAMED non-nil => Rename all frames named FRAME to NEWNAME.
OLD-NAME may be a frame, its name, or nil.  Default is `selected-frame'.
NEW-NAME is a string or nil.  Default NEW-NAME is current `buffer-name'.

\(fn &optional OLD-NAME NEW-NAME ALL-NAMED)" t nil)

(autoload 'show-frame "frame-cmds" "\
Make FRAME visible and raise it, without selecting it.
FRAME may be a frame or its name.

\(fn FRAME)" t nil)

(autoload 'hide-frame "frame-cmds" "\
Make FRAME invisible.  Like `make-frame-invisible', but reads frame name.
Non-nil PREFIX makes it invisible even if all other frames are invisible.

\(fn FRAME &optional PREFIX)" t nil)

(autoload 'show-a-frame-on "frame-cmds" "\
Make visible and raise a frame showing BUFFER, if there is one.
Neither the frame nor the BUFFER are selected.
BUFFER may be a buffer or its name (a string).

\(fn BUFFER)" t nil)

(autoload 'show-*Help*-buffer "frame-cmds" "\
Raise a frame showing buffer *Help*, without selecting it.

\(fn)" t nil)

(autoload 'delete-1-window-frames-on "frame-cmds" "\
Delete all visible 1-window frames showing BUFFER.

\(fn BUFFER)" t nil)

(autoload 'delete-other-frames "frame-cmds" "\
Delete all frames except FRAME (default: selected frame).
Interactively, use a prefix arg (`\\[universal-argument]') to be prompted for FRAME.

\(fn &optional FRAME)" t nil)

(autoload 'maximize-frame-horizontally "frame-cmds" "\
Maximize selected frame horizontally.

\(fn &optional FRAME)" t nil)

(autoload 'maximize-frame-vertically "frame-cmds" "\
Maximize selected frame vertically.

\(fn &optional FRAME)" t nil)

(autoload 'maximize-frame "frame-cmds" "\
Maximize selected frame horizontally, vertically, or both.
With no prefix arg, maximize both directions.
With a non-negative prefix arg, maximize vertically.
With a negative prefix arg, maximize horizontally.

In Lisp code:
 DIRECTION is the direction: `horizontal', `vertical', or `both'.
 FRAME is the frame to maximize.

\(fn &optional DIRECTION FRAME)" t nil)

(defalias 'restore-frame-horizontally 'toggle-max-frame-horizontally)

(autoload 'toggle-max-frame-horizontally "frame-cmds" "\
Toggle maximization of FRAME horizontally.
If used once, this restores the frame.  If repeated, it maximizes.
This affects the `left' and `width' frame parameters.

FRAME defaults to the selected frame.

\(fn &optional FRAME)" t nil)

(defalias 'restore-frame-horizontally 'toggle-max-frame-horizontally)

(autoload 'toggle-max-frame-vertically "frame-cmds" "\
Toggle maximization of FRAME vertically.
If used once, this restores the frame.  If repeated, it maximizes.
This affects the `top' and `height' frame parameters.

FRAME defaults to the selected frame.

\(fn &optional FRAME)" t nil)

(defalias 'restore-frame 'toggle-max-frame)

(autoload 'toggle-max-frame "frame-cmds" "\
Toggle maximization of FRAME horizontally, vertically, or both.
Reverses or (if restored) repeats the effect of the Emacs maximize
commands.  Does not restore from maximization effected outside Emacs.

With no prefix arg, toggle both directions.
With a non-negative prefix arg, toggle only vertically.
With a negative prefix arg, toggle horizontally.

When toggling both, each is toggled from its last maximize or restore
state.  This means that using this after `maximize-horizontal',
`maximize-vertical', `toggle-max-horizontal', or `toggle-max-vertical'
does not necessarily just reverse the effect of that command.

In Lisp code:
 DIRECTION is the direction: `horizontal', `vertical', or `both'.
 FRAME is the frame to change.  It defaults to the selected frame.

\(fn &optional DIRECTION FRAME)" t nil)

(autoload 'tile-frames-horizontally "frame-cmds" "\
Tile frames horizontally.
Interatively:
  With prefix arg, you are prompted for names of two frames to tile.
  With no prefix arg, all visible frames are tiled, except a
       standalone minibuffer frame, if any.
If called from a program, all frames in list FRAMES are tiled.

\(fn &optional FRAMES)" t nil)

(autoload 'tile-frames-vertically "frame-cmds" "\
Tile frames vertically.
Interatively:
  With prefix arg, you are prompted for names of two frames to tile.
  With no prefix arg, all visible frames are tiled, except a
       standalone minibuffer frame, if any.
If called from a program, all frames in list FRAMES are tiled.

\(fn &optional FRAMES)" t nil)

(autoload 'enlarge-frame "frame-cmds" "\
Increase the height of FRAME (default: selected-frame) by INCREMENT.
INCREMENT is in lines (characters).
Interactively, it is given by the prefix argument.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'enlarge-frame-horizontally "frame-cmds" "\
Increase the width of FRAME (default: selected-frame) by INCREMENT.
INCREMENT is in columns (characters).
Interactively, it is given by the prefix argument.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'shrink-frame "frame-cmds" "\
Decrease the height of FRAME (default: selected-frame) by INCREMENT.
INCREMENT is in lines (characters).
Interactively, it is given by the prefix argument.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'shrink-frame-horizontally "frame-cmds" "\
Decrease the width of FRAME (default: selected-frame) by INCREMENT.
INCREMENT is in columns (characters).
Interactively, it is given by the prefix argument.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'move-frame-down "frame-cmds" "\
Move FRAME (default: selected-frame) down by INCREMENT.
INCREMENT is in units of ten pixels.
Interactively, it is given by the prefix argument.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'move-frame-up "frame-cmds" "\
Move FRAME (default: selected-frame) up by INCREMENT.
INCREMENT is in units of ten pixels.
Interactively, it is given by the prefix argument.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'move-frame-right "frame-cmds" "\
Move FRAME (default: selected-frame) toward the right by INCREMENT.
INCREMENT is in units of ten pixels.
Interactively, it is given by the prefix argument.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'move-frame-left "frame-cmds" "\
Move FRAME (default: selected-frame) toward the left by INCREMENT.
INCREMENT is in units of ten pixels.
Interactively, it is given by the prefix argument.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'enlarge-font "frame-cmds" "\
Increase size of font in FRAME by INCREMENT.
Interactively, INCREMENT is given by the prefix argument.
Optional FRAME parameter defaults to current frame.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'set-frame-alist-parameter-from-frame "frame-cmds" "\
Set PARAMETER of frame alist ALIST to its current value in FRAME.
FRAME defaults to the selected frame.  ALIST is a variable (symbol)
whose value is an alist of frame parameters.

\(fn ALIST PARAMETER &optional FRAME)" t nil)

(autoload 'set-all-frame-alist-parameters-from-frame "frame-cmds" "\
Set frame parameters of ALIST to their current values in FRAME.
Unless optional argument REALLY-ALL-P (prefix arg) is non-nil, the
frame parameters in list `frame-parameters-to-exclude' are
excluded: they are not copied from FRAME to ALIST.
ALIST is a variable (symbol) whose value is an alist of frame parameters.
FRAME defaults to the selected frame.

\(fn ALIST &optional FRAME REALLY-ALL-P)" t nil)

(autoload 'tell-customize-var-has-changed "frame-cmds" "\
Tell Customize to recognize that VARIABLE has been set (changed).
VARIABLE is a symbol that names a user option.

\(fn VARIABLE)" t nil)

(autoload 'other-window-or-frame "frame-cmds" "\
`other-frame', if `one-window-p'; otherwise, `other-window'.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (haml-mode) "haml-mode" "haml-mode.el" (19823 61956))
;;; Generated autoloads from haml-mode.el

(autoload 'haml-mode "haml-mode" "\
Major mode for editing Haml files.

\\{haml-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

;;;***

;;;### (autoloads (update-file-header make-box-comment make-divider
;;;;;;  make-revision make-header) "header2" "header2.el" (19807
;;;;;;  38146))
;;; Generated autoloads from header2.el

(autoload 'make-header "header2" "\
Insert (mode-dependent) header comment at beginning of file.
A header is composed of a mode line, a body, and an end line.  The body is
constructed by calling the functions in `make-header-hook'.  The mode line
and end lines start and terminate block comments.  The body lines continue
the comment.

\(fn)" t nil)

(autoload 'make-revision "header2" "\
Prepare for a new history revision.  Insert history line if inexistant.

\(fn)" t nil)

(autoload 'make-divider "header2" "\
Insert a comment divider line: the comment start, filler, and end.
END-COL is the last column of the divider line.

\(fn &optional END-COL)" t nil)

(autoload 'make-box-comment "header2" "\
Insert an empty (mode dependent) box comment.
END-COL is the last column of the divider line.

\(fn &optional END-COL)" t nil)

(autoload 'update-file-header "header2" "\
Update file header.
Search the first `header-max' chars in buffer using regexps in
`file-header-update-alist'.  When a match is found, apply the
corresponding function with point located just after the match.
The functions can use `match-beginning' and `match-end' to find
the strings that cause them to be invoked.

\(fn)" t nil)

;;;***

;;;### (autoloads (highlight-parentheses-mode) "highlight-parentheses"
;;;;;;  "highlight-parentheses.el" (19815 53070))
;;; Generated autoloads from highlight-parentheses.el

(autoload 'highlight-parentheses-mode "highlight-parentheses" "\
Minor mode to highlight the surrounding parentheses.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (hl-line-flash hl-line-when-idle-interval hl-line-toggle-when-idle
;;;;;;  hl-line-inhibit-highlighting-for-modes hl-line-flash-show-period)
;;;;;;  "hl-line+" "hl-line+.el" (19804 63413))
;;; Generated autoloads from hl-line+.el

(defface hl-line '((t (:background "SlateGray3"))) "\
Face to use for `hl-line-face'." :group (quote hl-line))

(defvar hl-line-flash-show-period 1 "\
Number of seconds for `hl-line-flash' to highlight the line.")

(custom-autoload 'hl-line-flash-show-period "hl-line+" t)

(defvar hl-line-inhibit-highlighting-for-modes nil "\
Modes where highlighting is inhibited for `hl-line-highlight-now'.
A list of `major-mode' values (symbols).")

(custom-autoload 'hl-line-inhibit-highlighting-for-modes "hl-line+" t)

(defalias 'toggle-hl-line-when-idle 'hl-line-toggle-when-idle)

(autoload 'hl-line-toggle-when-idle "hl-line+" "\
Turn on or off using `global-hl-line-mode' when Emacs is idle.
When on, use `global-hl-line-mode' whenever Emacs is idle.
With prefix argument, turn on if ARG > 0; else turn off.

\(fn &optional ARG)" t nil)

(autoload 'hl-line-when-idle-interval "hl-line+" "\
Set wait until using `global-hl-line-mode' when Emacs is idle.
Whenever Emacs is idle for this many seconds, `global-hl-line-mode'
will be turned on.

To turn on or off using `global-hl-line-mode' when idle,
use `\\[toggle-hl-line-when-idle].

\(fn SECS)" t nil)

(defalias 'flash-line-highlight 'hl-line-flash)

(autoload 'hl-line-flash "hl-line+" "\
Highlight the current line for `hl-line-flash-show-period' seconds.
With a prefix argument, highlight for that many seconds.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (iimage-mode turn-on-iimage-mode) "iimage" "iimage.el"
;;;;;;  (19823 30503))
;;; Generated autoloads from iimage.el

(autoload 'turn-on-iimage-mode "iimage" "\
Unconditionally turn on iimage mode.

\(fn)" t nil)

(autoload 'iimage-mode "iimage" "\
Toggle inline image minor mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (run-ruby inf-ruby inf-ruby-keys) "inf-ruby" "rinari/util/inf-ruby.el"
;;;;;;  (19809 16085))
;;; Generated autoloads from rinari/util/inf-ruby.el

(autoload 'inf-ruby-keys "inf-ruby" "\
Set local key defs to invoke inf-ruby from ruby-mode.

\(fn)" nil nil)

(autoload 'inf-ruby "inf-ruby" "\
Run an inferior Ruby process in a buffer.
With prefix argument, prompts for which Ruby implementation
\(from the list `inf-ruby-implementations') to use. Runs the
hooks `inf-ruby-mode-hook' (after the `comint-mode-hook' is
run).

\(fn &optional IMPL)" t nil)

(autoload 'run-ruby "inf-ruby" "\
Run an inferior Ruby process, input and output via buffer *ruby*.
If there is a process already running in `*ruby*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `ruby-program-name').  Runs the hooks `inferior-ruby-mode-hook'
\(after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)

\(fn &optional COMMAND NAME)" t nil)

(eval-after-load 'ruby-mode '(add-hook 'ruby-mode-hook 'inf-ruby-keys))

;;;***

;;;### (autoloads (pluralize-string singularize-string) "inflections"
;;;;;;  "rinari/util/jump/inflections.el" (19809 16109))
;;; Generated autoloads from rinari/util/jump/inflections.el

(autoload 'singularize-string "inflections" "\
Not documented

\(fn STR)" nil nil)

(autoload 'pluralize-string "inflections" "\
Not documented

\(fn STR)" nil nil)

;;;***

;;;### (autoloads (inversion-upgrade-package inversion-add-to-load-path
;;;;;;  inversion-find-version inversion-require-emacs inversion-require)
;;;;;;  "inversion" "cedet/common/inversion.el" (19155 62054))
;;; Generated autoloads from cedet/common/inversion.el

(autoload 'inversion-require "inversion" "\
Declare that you need PACKAGE with at least VERSION.
PACKAGE might be found in FILE.  (See `require'.)
Throws an error if VERSION is incompatible with what is installed.
Optional argument DIRECTORY is a location where new versions of
this tool can be located.  If there is a versioning problem and
DIRECTORY is provided, inversion will offer to download the file.
Optional argument RESERVED is saved for later use.

\(fn PACKAGE VERSION &optional FILE DIRECTORY &rest RESERVED)" nil nil)

(autoload 'inversion-require-emacs "inversion" "\
Declare that you need either EMACS-VER, or XEMACS-VER.
Only checks one based on which kind of Emacs is being run.

\(fn EMACS-VER XEMACS-VER)" nil nil)

(autoload 'inversion-find-version "inversion" "\
Search for the version and incompatible version of PACKAGE.
Does not load PACKAGE nor requires that it has been previously loaded.
Search in the directories in `load-path' for a PACKAGE.el library.
Visit the file found and search for the declarations of variables or
constants `PACKAGE-version' and `PACKAGE-incompatible-version'.  The
value of these variables must be a version string.

Return a pair (VERSION-STRING . INCOMPATIBLE-VERSION-STRING) where
INCOMPATIBLE-VERSION-STRING can be nil.
Return nil when VERSION-STRING was not found.

\(fn PACKAGE)" nil nil)

(autoload 'inversion-add-to-load-path "inversion" "\
Add the PACKAGE path to `load-path' if necessary.
MINIMUM is the minimum version requirement of PACKAGE.
Optional argument INSTALLDIR is the base directory where PACKAGE is
installed.  It defaults to `default-directory'/PACKAGE.
SUBDIRS are sub-directories to add to `load-path', following the main
INSTALLDIR path.

\(fn PACKAGE MINIMUM &optional INSTALLDIR &rest SUBDIRS)" nil nil)

(autoload 'inversion-upgrade-package "inversion" "\
Try to upgrade PACKAGE in DIRECTORY is available.

\(fn PACKAGE &optional DIRECTORY)" t nil)

;;;***

;;;### (autoloads (inferior-js-mode switch-to-js js-load-file-and-go
;;;;;;  js-load-file js-send-buffer-and-go js-send-buffer js-send-last-sexp
;;;;;;  js-send-last-sexp-and-go js-send-region-and-go js-send-region
;;;;;;  run-js) "js-comint" "js-comint.el" (18762 18530))
;;; Generated autoloads from js-comint.el

(autoload 'run-js "js-comint" "\
Run an inferior Javascript process, input and output via buffer `*js*'.
If there is a process already running in `*js*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `inferior-js-program-command').
Runs the hook `inferior-js-mode-hook' (after the `comint-mode-hook'
is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)

\(fn CMD &optional DONT-SWITCH-P)" t nil)

(autoload 'js-send-region "js-comint" "\
Send the current region to the inferior Javascript process.

\(fn START END)" t nil)

(autoload 'js-send-region-and-go "js-comint" "\
Send the current region to the inferior Javascript process.

\(fn START END)" t nil)

(autoload 'js-send-last-sexp-and-go "js-comint" "\
Send the previous sexp to the inferior Js process.

\(fn)" t nil)

(autoload 'js-send-last-sexp "js-comint" "\
Send the previous sexp to the inferior Javascript process.

\(fn)" t nil)

(autoload 'js-send-buffer "js-comint" "\
Send the buffer to the inferior Javascript process.

\(fn)" t nil)

(autoload 'js-send-buffer-and-go "js-comint" "\
Send the buffer to the inferior Javascript process.

\(fn)" t nil)

(autoload 'js-load-file "js-comint" "\
Load a file in the javascript interpreter.

\(fn FILENAME)" t nil)

(autoload 'js-load-file-and-go "js-comint" "\
Load a file in the javascript interpreter.

\(fn FILENAME)" t nil)

(autoload 'switch-to-js "js-comint" "\
Switch to the javascript process buffer.
With argument, position cursor at end of buffer.

\(fn EOB-P)" t nil)

(autoload 'inferior-js-mode "js-comint" "\
Major mode for interacting with an inferior javascript process.

The following commands are available:
\\{inferior-js-mode-map}

A javascript process can be fired up with M-x run-js.

Customization: Entry to this mode runs the hooks on comint-mode-hook and
inferior-js-mode-hook (in that order).

You can send text to the inferior Javascript process from othber buffers containing
Javascript source.
    switch-to-js switches the current buffer to the Javascript process buffer.
    js-send-region sends the current region to the Javascript process.


\(fn)" t nil)

;;;***

;;;### (autoloads (js2-mode) "js2-mode" "js2-mode.el" (18804 37919))
;;; Generated autoloads from js2-mode.el
 (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(autoload 'js2-mode "js2-mode" "\
Major mode for editing JavaScript code.

\(fn)" t nil)

;;;***

;;;### (autoloads (defjump) "jump" "rinari/util/jump/jump.el" (19809
;;;;;;  16109))
;;; Generated autoloads from rinari/util/jump/jump.el

(autoload 'defjump "jump" "\
Define NAME as a function with behavior determined by SPECS.
SPECS should be a list of cons cells of the form

   (jump-from-spec . jump-to-spec)

NAME will then try subsequent jump-from-specs until one succeeds,
at which point any resulting match information, along with the
related jump-to-spec will be used to jump to the intended buffer.
See `jump-to' and `jump-from' for information on spec
construction.

ROOT should specify the root of the project in which all jumps
take place, it can be either a string directory path, or a
function returning

Optional argument DOC specifies the documentation of the
resulting function.

Optional argument MAKE can be used to specify that missing files
should be created.  If MAKE is a function then it will be called
with the file path as it's only argument.  After possibly calling
MAKE `find-file' will be used to open the path.

Optional argument METHOD-COMMAND overrides the function used to
find the current method which defaults to `which-function'.

\(fn NAME SPECS ROOT &optional DOC MAKE METHOD-COMMAND)" nil nil)

;;;***

;;;### (autoloads (latex-math-decode-buffer latex-math-decode-region)
;;;;;;  "latex-math-symbol" "mu-cite-8.1/latex-math-symbol.el" (14226
;;;;;;  42008))
;;; Generated autoloads from mu-cite-8.1/latex-math-symbol.el

(autoload 'latex-math-decode-region "latex-math-symbol" "\
Not documented

\(fn BEG END)" t nil)

(autoload 'latex-math-decode-buffer "latex-math-symbol" "\
Not documented

\(fn)" t nil)

;;;***

;;;### (autoloads (insert-lib-requires-as-comment lib-requires lib-requires-tree
;;;;;;  lib-requires-header) "lib-requires" "lib-requires.el" (18277
;;;;;;  44528))
;;; Generated autoloads from lib-requires.el

(defvar lib-requires-header ";; Features that might be required by this library:\n;;\n" "\
*Header inserted by `insert-lib-requires-as-comment'.")

(custom-autoload 'lib-requires-header "lib-requires" t)

(autoload 'lib-requires-tree "lib-requires" "\
The features `require'd by LIBRARY, as a tree.
The tree structure shows library dependencies: Each feature is
represented by its name or by a list of its name followed by the
features that it explicitly requires.

Argument LIBRARY is an Emacs-Lisp file name, or file name sans
extension.  This command loads LIBRARY before determining its
dependencies.  This means that LIBRARY must contain (provide LIBRARY).
If it does not, an error is raised.

Function `lib-requires-tree' calls itself recursively on its
dependencies, so an attempt is made to load all of them.

Note: If a byte-compiled (`*.elc') version of a library is
available, it is loaded, in preference to the source library -
this is the standard behavior of `load-library'.  This means that
the tree of required features reflects the dependencies indicated
in the byte-compiled file, not the source file.  If the
byte-compiled file is out-of-date, so will be the result of
`lib-requires-tree'.

A required feature that was loaded successfully is represented by a
  string that names the required feature.
A required file or feature that failed to load is represented by a
  symbol that names the required feature.

For example: Suppose that library `doremi.el' requires `ring+' and
`mwheel', and library `ring+' requires `ring'.  If `ring+' is
successfully loaded and `mwheel.el' is not, then the result is this:

  (mwheel (\"ring+\" (\"ring\")))

Argument CUMUL is used only for recursive calls, to accumulate the
required features.

See also command `lib-requires'.

Note that `lib-requires-tree' and `lib-requires' are roughly the
opposite of `file-dependents' in library `loadhist'.

\(fn LIBRARY &optional CUMUL)" t nil)

(autoload 'lib-requires "lib-requires" "\
The libraries ultimately `require'd by LIBRARY, as a flat list.
Each library (file or feature) is represented only once, and the list
is sorted.

A library is represented as for `lib-requires-tree': a file-name
string for a successfully loaded required library, a feature-name
symbol for an unsuccessfully loaded required feature.

LIBRARY must contain (provide LIBRARY); otherwise, an error is raised.

Note that `lib-requires-tree' and `lib-requires' are essentially the
opposite of `file-dependents' in library `loadhist'.

\(fn LIBRARY)" t nil)

(autoload 'insert-lib-requires-as-comment "lib-requires" "\
Insert a comment listing all libraries ultimately required by LIBRARY.
See also `lib-requires' and `lib-requires-tree'.

\(fn LIBRARY)" t nil)

;;;***

;;;### (autoloads (enable-visual-studio-bookmarks) "linemark" "cedet/eieio/linemark.el"
;;;;;;  (18791 54624))
;;; Generated autoloads from cedet/eieio/linemark.el

(autoload 'enable-visual-studio-bookmarks "linemark" "\
Bind the viss bookmark functions to F2 related keys.
\\<global-map>
\\[viss-bookmark-toggle]     - To=ggle a bookmark on this line.
\\[viss-bookmark-next-buffer]   - Move to the next bookmark.
\\[viss-bookmark-prev-buffer]   - Move to the previous bookmark.
\\[viss-bookmark-clear-all-buffer] - Clear all bookmarks.

\(fn)" t nil)

;;;***

;;;### (autoloads (linum-version linum linum-format) "linum" "linum.el"
;;;;;;  (18308 112))
;;; Generated autoloads from linum.el

(defvar linum-format "%6d  " "\
Format used to display line numbers. Either a format string like \"%6d  \", 
or the symbol 'dynamic to adapt the width as needed. 'dynamic or 
a format string that does not expand to a multiple of 8 can make 
indentations look different if you indent using tab characters.")

(custom-autoload 'linum-format "linum" t)

(autoload 'linum "linum" "\
Toggle display of line numbers.

\(fn)" t nil)

(autoload 'linum-version "linum" "\
Display version of linum.

\(fn)" t nil)

;;;***

;;;### (autoloads (lmcompile-do-highlight) "lmcompile" "cedet/eieio/lmcompile.el"
;;;;;;  (18791 54642))
;;; Generated autoloads from cedet/eieio/lmcompile.el

(autoload 'lmcompile-do-highlight "lmcompile" "\
Do compilation mode highlighting.
Works on grep, compile, or other type mode.

\(fn)" t nil)

;;;***

;;;### (autoloads (enable-lui-irc-colors) "lui-irc-colors" "circe/lui-irc-colors.el"
;;;;;;  (18087 33243))
;;; Generated autoloads from circe/lui-irc-colors.el

(autoload 'enable-lui-irc-colors "lui-irc-colors" "\
Enable IRC color interpretation for Lui.

\(fn)" t nil)

;;;***

;;;### (autoloads (mc-deactivate-passwd mc-install-write-mode mc-install-read-mode
;;;;;;  mc-read-mode) "mailcrypt" "mailcrypt-3.5.9/mailcrypt.el"
;;;;;;  (19352 62013))
;;; Generated autoloads from mailcrypt-3.5.9/mailcrypt.el

(autoload 'mc-read-mode "mailcrypt" "\

Minor mode for interfacing with cryptographic functions.
\\<mc-read-mode-map>
\\[mc-decrypt]		Decrypt an encrypted message
\\[mc-verify]		Verify signature on a clearsigned message
\\[mc-snarf]		Add public key(s) to keyring
\\[mc-fetch-key]		Fetch a PGP key via finger or HTTP
\\[mc-deactivate-passwd]		Forget passphrase(s)

\(fn &optional ARG)" t nil)

(autoload 'mc-install-read-mode "mailcrypt" "\
Not documented

\(fn)" t nil)

(autoload 'mc-install-write-mode "mailcrypt" "\
Not documented

\(fn)" t nil)

(autoload 'mc-deactivate-passwd "mailcrypt" "\
*Deactivate the passphrase cache.

\(fn &optional INHIBIT-MESSAGE)" t nil)

;;;***

;;;### (autoloads (mars-tiling-favorite-secondary-layouts mars-tiling-favorite-main-layouts
;;;;;;  mars-tiling-master-top mars-tiling-master-left mars-tiling)
;;;;;;  "mars-tiling" "mars-tiling.el" (19807 54291))
;;; Generated autoloads from mars-tiling.el

(let ((loads (get 'mars-tiling 'custom-loads))) (if (member '"mars-tiling" loads) nil (put 'mars-tiling 'custom-loads (cons '"mars-tiling" loads))))

(defvar mars-tiling-master-left 70 "\
Ratio of the left window used in `tiling-master-left'. As percent unit, between 0 and 100.")

(custom-autoload 'mars-tiling-master-left "mars-tiling" t)

(defvar mars-tiling-master-top 70 "\
Ratio of the top window used in `tiling-master-top'. As percent unit, between 0 and 100.")

(custom-autoload 'mars-tiling-master-top "mars-tiling" t)

(defvar mars-tiling-favorite-main-layouts (list 'tiling-master-left 'tiling-tile-4) "\
Main tiling layouts mainly used in `tiling-cycle'. An ordered list of functions.")

(custom-autoload 'mars-tiling-favorite-main-layouts "mars-tiling" t)

(defvar mars-tiling-favorite-secondary-layouts (list 'tiling-master-top 'tiling-tile-4) "\
Secondary tiling layouts mainly used in `tiling-cycle'. An ordered list of functions.")

(custom-autoload 'mars-tiling-favorite-secondary-layouts "mars-tiling" t)

(defadvice tiling-cycle (around special-layouts-tiling (&optional numOfWins special-layouts) activate) (interactive "P") (let ((tiling-layouts (if (null special-layouts) tiling-layouts special-layouts))) ad-do-it))

(defadvice tiling-master (around mars-tiling-master (bufs horizontal) activate) "\
redefine 'tiling-master to expand left and top window to a willing size in percent
using `mars-tiling-master-left' and `mars-tiling-master-top' respectively. No more
minibuffer resizing (with new BALANCE-WINDOWS)." ad-do-it (flet ((crop-percent (num) (cond ((> num 100) 100) ((< num 0) 1) (t num))) (find-new-size (ratio orig-size) (floor (* orig-size (/ ratio 100.0))))) (if horizontal (let ((w-factor (- (find-new-size (crop-percent mars-tiling-master-left) (frame-width (selected-frame))) (window-width)))) (enlarge-window-horizontally w-factor)) (let ((h-factor (- (find-new-size (crop-percent mars-tiling-master-top) (frame-height (selected-frame))) (window-height)))) (enlarge-window h-factor)))))

;;;***

;;;### (autoloads (kiwon/restore-window-configuration kiwon/save-window-configuration
;;;;;;  mars-windows-archiver-load-in-session mars-windows-archiver-load
;;;;;;  mars-windows-archiver-restore mars-windows-archiver-clear
;;;;;;  mars-windows-archiver-save kiwon/last-window-configuration-file
;;;;;;  mars-windows-register-limit mars-windows-archiver-file configuration-file-name
;;;;;;  mars-windows-archiver) "mars-windows-archiver" "mars-windows-archiver.el"
;;;;;;  (19813 33195))
;;; Generated autoloads from mars-windows-archiver.el

(let ((loads (get 'mars-windows-archiver 'custom-loads))) (if (member '"mars-windows-archiver" loads) nil (put 'mars-windows-archiver 'custom-loads (cons '"mars-windows-archiver" loads))))

(autoload 'configuration-file-name "mars-windows-archiver" "\
Generates a complete name for a configuration file according to the `Emacs' version.

\(fn NAME &optional DIR)" nil nil)

(defvar mars-windows-archiver-file (configuration-file-name "windows-archiver" "data") "\
*File name where window configurations are saved to and loaded from.

If you want your window configurations shared between Emacs and XEmacs,
customize this value and make sure that `mars-windows-archiver-coding-system'
is set to a coding system that exists in both emacsen.")

(custom-autoload 'mars-windows-archiver-file "mars-windows-archiver" t)

(defvar mars-windows-register-limit 10 "\
number of slots in the `mars-windows-register'.")

(custom-autoload 'mars-windows-register-limit "mars-windows-archiver" t)

(defvar kiwon/last-window-configuration-file (configuration-file-name "last-window-configuration" "data") "\
Script to restore the window configuration at startup.")

(custom-autoload 'kiwon/last-window-configuration-file "mars-windows-archiver" t)

(autoload 'mars-windows-archiver-save "mars-windows-archiver" "\
Not documented

\(fn &optional DONT-ALERT)" t nil)

(autoload 'mars-windows-archiver-clear "mars-windows-archiver" "\
Not documented

\(fn)" t nil)

(autoload 'mars-windows-archiver-restore "mars-windows-archiver" "\
Not documented

\(fn &optional NUM)" t nil)

(autoload 'mars-windows-archiver-load "mars-windows-archiver" "\
Not documented

\(fn)" t nil)

(autoload 'mars-windows-archiver-load-in-session "mars-windows-archiver" "\
Not documented

\(fn)" nil nil)

(autoload 'kiwon/save-window-configuration "mars-windows-archiver" "\
Not documented

\(fn)" nil nil)

(autoload 'kiwon/restore-window-configuration "mars-windows-archiver" "\
Not documented

\(fn)" nil nil)

;;;***

;;;### (autoloads (mc-gpg-fetch-from-finger mc-gpg-fetch-key) "mc-gpg"
;;;;;;  "mailcrypt-3.5.9/mc-gpg.el" (19352 62013))
;;; Generated autoloads from mailcrypt-3.5.9/mc-gpg.el

(autoload 'mc-gpg-fetch-key "mc-gpg" "\
Fetch a key using the gpg --recv-key method. With this method it is
only possible to look for key ids!

\(fn &optional ID)" t nil)

(autoload 'mc-gpg-fetch-from-finger "mc-gpg" "\
Fetch a key from a finger server. This function takes one argument of the
form USER@HOST.

\(fn ID)" t nil)

;;;***

;;;### (autoloads (mc-pgp-fetch-key) "mc-pgp" "mailcrypt-3.5.9/mc-pgp.el"
;;;;;;  (19352 62013))
;;; Generated autoloads from mailcrypt-3.5.9/mc-pgp.el

(autoload 'mc-pgp-fetch-key "mc-pgp" "\
Attempt to fetch a key for addition to PGP keyring.  Interactively,
prompt for string matching key to fetch.

Non-interactively, ID must be a pair.  The CAR must be a bare Email
address and the CDR a keyID (with \"0x\" prefix).  Either, but not
both, may be nil.

Return t if we think we were successful; nil otherwise.  Note that nil
is not necessarily an error, since we may have merely fired off an Email
request for the key.

\(fn &optional ID)" t nil)

;;;***

;;;### (autoloads (mc-pgp50-fetch-key) "mc-pgp5" "mailcrypt-3.5.9/mc-pgp5.el"
;;;;;;  (19352 62013))
;;; Generated autoloads from mailcrypt-3.5.9/mc-pgp5.el

(autoload 'mc-pgp50-fetch-key "mc-pgp5" "\
Attempt to fetch a key for addition to PGP keyring.  Interactively,
prompt for string matching key to fetch.

Non-interactively, ID must be a pair.  The CAR must be a bare Email
address and the CDR a keyID (with \"0x\" prefix).  Either, but not
both, may be nil.

Return t if we think we were successful; nil otherwise.  Note that nil
is not necessarily an error, since we may have merely fired off an Email
request for the key.

\(fn &optional ID)" t nil)

;;;***

;;;### (autoloads (mc-pgp65-fetch-key) "mc-pgp6" "mailcrypt-3.5.9/mc-pgp6.el"
;;;;;;  (19352 62013))
;;; Generated autoloads from mailcrypt-3.5.9/mc-pgp6.el

(autoload 'mc-pgp65-fetch-key "mc-pgp6" "\
Attempt to fetch a key for addition to PGP keyring.  Interactively,
prompt for string matching key to fetch.

Non-interactively, ID must be a pair.  The CAR must be a bare Email
address and the CDR a keyID (with \"0x\" prefix).  Either, but not
both, may be nil.

Return t if we think we were successful; nil otherwise.  Note that nil
is not necessarily an error, since we may have merely fired off an Email
request for the key.

\(fn &optional ID)" t nil)

;;;***

;;;### (autoloads (mc-remailer-insert-response-block mc-remailer-encrypt-for-chain
;;;;;;  mc-remailer-insert-pseudonym mc-reread-levien-file) "mc-remail"
;;;;;;  "mailcrypt-3.5.9/mc-remail.el" (19352 62013))
;;; Generated autoloads from mailcrypt-3.5.9/mc-remail.el

(autoload 'mc-reread-levien-file "mc-remail" "\
Read the Levien format file specified in `mc-levien-file-name'.

Place result in `mc-remailer-internal-chains' and `mc-remailer-internal-ranking'.

See the documentation for the variable `mc-levien-file-name' for
a description of Levien file format.

\(fn)" t nil)

(autoload 'mc-remailer-insert-pseudonym "mc-remail" "\
Insert pseudonym as a From field in the hash-mark header.

See the documentation for the variable `mc-remailer-pseudonyms' for
more information.

\(fn)" t nil)

(autoload 'mc-remailer-encrypt-for-chain "mc-remail" "\
Encrypt message for a remailer chain, prompting for chain to use.

With \\[universal-argument] \\[universal-argument], pause before each
encryption.

\(fn &optional PAUSE)" t nil)

(autoload 'mc-remailer-insert-response-block "mc-remail" "\
Insert response block at point, prompting for chain to use.

With \\[universal-argument], enter a recursive edit of the innermost
layer of the block before encrypting it.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (mc-setversion) "mc-setversion" "mailcrypt-3.5.9/mc-setversion.el"
;;;;;;  (19352 62013))
;;; Generated autoloads from mailcrypt-3.5.9/mc-setversion.el

(autoload 'mc-setversion "mc-setversion" "\
Reset path and argument information for the selected version of PGP.
Possible values of VERSION are 2.6, 5.0, 6.5, and gpg.

\(fn &optional VERSION)" t nil)

;;;***

;;;### (autoloads (mc-mew-decrypt-message mc-mew-summary-snarf-keys
;;;;;;  mc-mew-summary-verify-signature mc-mew-summary-decrypt-message
;;;;;;  mc-mh-snarf-keys mc-mh-verify-signature mc-mh-decrypt-message
;;;;;;  mc-gnus-decrypt-message mc-gnus-snarf-keys mc-gnus-verify-signature
;;;;;;  mc-vm-snarf-keys mc-vm-decrypt-message mc-vm-verify-signature
;;;;;;  mc-rmail-view-quit mc-rmail-decrypt-message mc-rmail-verify-signature
;;;;;;  mc-rmail-summary-snarf-keys mc-rmail-summary-decrypt-message
;;;;;;  mc-rmail-summary-verify-signature mc-fetch-key mc-snarf-keys
;;;;;;  mc-snarf mc-insert-public-key mc-remail-region mc-remail
;;;;;;  mc-verify mc-sign-region mc-sign mc-decrypt mc-encrypt-region
;;;;;;  mc-encrypt) "mc-toplev" "mailcrypt-3.5.9/mc-toplev.el" (19352
;;;;;;  62013))
;;; Generated autoloads from mailcrypt-3.5.9/mc-toplev.el

(autoload 'mc-encrypt "mc-toplev" "\
*Encrypt the current buffer.

Exact behavior depends on current major mode.

With \\[universal-argument], prompt for User ID to sign as.

With \\[universal-argument] \\[universal-argument], prompt for encryption scheme to use.

\(fn ARG)" t nil)

(autoload 'mc-encrypt-region "mc-toplev" "\
*Encrypt the current region.

\(fn ARG START END)" t nil)

(autoload 'mc-decrypt "mc-toplev" "\
*Decrypt a message in the current buffer.

Exact behavior depends on current major mode.

\(fn)" t nil)

(autoload 'mc-sign "mc-toplev" "\
*Sign a message in the current buffer.

Exact behavior depends on current major mode.

With one prefix arg, prompts for private key to use, with two prefix args,
also prompts for encryption scheme to use.  With negative prefix arg,
inhibits clearsigning (pgp).

\(fn ARG)" t nil)

(autoload 'mc-sign-region "mc-toplev" "\
*Sign the current region.

\(fn ARG START END)" t nil)

(autoload 'mc-verify "mc-toplev" "\
*Verify a message in the current buffer.

Exact behavior depends on current major mode.

\(fn)" t nil)

(autoload 'mc-remail "mc-toplev" "\
*Prepare the current buffer for delivery through an anonymous remailer.

Exact behavior depends on current major mode.

With \\[universal-argument], show verbose progress during remailing.

With \\[universal-argument] \\[universal-argument], prompt for remailer scheme to use.

\(fn ARG)" t nil)

(autoload 'mc-remail-region "mc-toplev" "\
*Deliver the region to an anonymous remailer.

\(fn ARG START END)" t nil)

(autoload 'mc-insert-public-key "mc-toplev" "\
*Insert your public key at point.
With one prefix arg, prompts for user id to use. With two prefix
args, prompts for encryption scheme.

\(fn &optional USERID SCHEME)" t nil)

(autoload 'mc-snarf "mc-toplev" "\
*Add all public keys in the buffer to your keyring.

Exact behavior depends on current major mode.

\(fn)" t nil)

(autoload 'mc-snarf-keys "mc-toplev" "\
*Add all public keys in the buffer to your keyring.

\(fn)" t nil)

(autoload 'mc-fetch-key "mc-toplev" "\
Fetch key with specified id from a server.

\(fn &optional ID)" t nil)

(autoload 'mc-rmail-summary-verify-signature "mc-toplev" "\
*Verify the signature in the current message.

\(fn)" t nil)

(autoload 'mc-rmail-summary-decrypt-message "mc-toplev" "\
*Decrypt the contents of this message

\(fn)" t nil)

(autoload 'mc-rmail-summary-snarf-keys "mc-toplev" "\
*Adds keys from current message to public key ring

\(fn)" t nil)

(autoload 'mc-rmail-verify-signature "mc-toplev" "\
*Verify the signature in the current message.

\(fn)" t nil)

(autoload 'mc-rmail-decrypt-message "mc-toplev" "\
*Decrypt the contents of this message

\(fn)" t nil)

(autoload 'mc-rmail-view-quit "mc-toplev" "\
Not documented

\(fn)" t nil)

(autoload 'mc-vm-verify-signature "mc-toplev" "\
*Verify the signature in the current VM message

\(fn)" t nil)

(autoload 'mc-vm-decrypt-message "mc-toplev" "\
*Decrypt the contents of the current VM message

\(fn)" t nil)

(autoload 'mc-vm-snarf-keys "mc-toplev" "\
*Snarf public key from the contents of the current VM message

\(fn)" t nil)

(autoload 'mc-gnus-verify-signature "mc-toplev" "\
Not documented

\(fn)" t nil)

(autoload 'mc-gnus-snarf-keys "mc-toplev" "\
Not documented

\(fn)" t nil)

(autoload 'mc-gnus-decrypt-message "mc-toplev" "\
Not documented

\(fn)" t nil)

(autoload 'mc-mh-decrypt-message "mc-toplev" "\
Decrypt the contents of the current MH message in the show buffer.

\(fn)" t nil)

(autoload 'mc-mh-verify-signature "mc-toplev" "\
*Verify the signature in the current MH message.

\(fn)" t nil)

(autoload 'mc-mh-snarf-keys "mc-toplev" "\
Not documented

\(fn)" t nil)

(autoload 'mc-mew-summary-decrypt-message "mc-toplev" "\
*Decrypt the current message

\(fn)" t nil)

(autoload 'mc-mew-summary-verify-signature "mc-toplev" "\
*Verify the signature in the current message.

\(fn)" t nil)

(autoload 'mc-mew-summary-snarf-keys "mc-toplev" "\
*Add keys from the current message to the public keyring.

\(fn)" t nil)

(autoload 'mc-mew-decrypt-message "mc-toplev" "\
*Decrypt the contents of this message.

\(fn)" t nil)

;;;***

;;;### (autoloads (mew-shimbun-expire mew-shimbun-expire-all mew-shimbun-re-retrieve-all
;;;;;;  mew-shimbun-re-retrieve mew-shimbun-retrieve-all mew-shimbun-retrieve
;;;;;;  mew-shimbun-goto-folder mew-shimbun-goto-unseen-folder) "mew-shimbun"
;;;;;;  "emacs-w3m/shimbun/mew-shimbun.el" (18197 61166))
;;; Generated autoloads from emacs-w3m/shimbun/mew-shimbun.el

(autoload 'mew-shimbun-goto-unseen-folder "mew-shimbun" "\
Goto folder for SHIMBUN to have a few new messages.

\(fn)" t nil)

(autoload 'mew-shimbun-goto-folder "mew-shimbun" "\
Goto folder for SHIMBUN.
If called with '\\[universal-argument]', goto folder to have a few new messages.

\(fn &optional ARGS)" t nil)

(autoload 'mew-shimbun-retrieve "mew-shimbun" "\
Retrieve articles via SHIMBUN on this folder.

\(fn &optional NEWFLD)" t nil)

(autoload 'mew-shimbun-retrieve-all "mew-shimbun" "\
Retrieve all articles via SHIMBUN.

\(fn)" t nil)

(autoload 'mew-shimbun-re-retrieve "mew-shimbun" "\
Re-retrieve this message.
If called with '\\[universal-argument]', re-retrieve messages marked with
'mew-shimbun-mark-re-retrieve'.

\(fn &optional ARGS)" t nil)

(autoload 'mew-shimbun-re-retrieve-all "mew-shimbun" "\
Re-retrieve all messages in this folder.
If called with '\\[universal-argument]', re-retrieve messages in the region.

\(fn &optional ARG)" t nil)

(autoload 'mew-shimbun-expire-all "mew-shimbun" "\
Expire all shimbun folder.

\(fn)" t nil)

(autoload 'mew-shimbun-expire "mew-shimbun" "\
Expire this shimbun folder.

\(fn)" t nil)

;;;***

;;;### (autoloads (mhc-cmail-setup) "mhc-cmail" "mhc/emacs/mhc-cmail.el"
;;;;;;  (18364 61487))
;;; Generated autoloads from mhc/emacs/mhc-cmail.el

(autoload 'mhc-cmail-setup "mhc-cmail" "\
Not documented

\(fn)" t nil)

;;;***

;;;### (autoloads (mhc-gnus-setup) "mhc-gnus" "mhc/emacs/mhc-gnus.el"
;;;;;;  (18364 61487))
;;; Generated autoloads from mhc/emacs/mhc-gnus.el

(autoload 'mhc-gnus-setup "mhc-gnus" "\
Not documented

\(fn)" nil nil)

;;;***

;;;### (autoloads (mhc-mew-setup) "mhc-mew" "mhc/emacs/mhc-mew.el"
;;;;;;  (18914 38510))
;;; Generated autoloads from mhc/emacs/mhc-mew.el

(autoload 'mhc-mew-setup "mhc-mew" "\
Not documented

\(fn)" nil nil)

;;;***

;;;### (autoloads (mhc-ps-insert-buffer mhc-ps-save mhc-ps-print
;;;;;;  mhc-ps-preview mhc-ps) "mhc-ps" "mhc/emacs/mhc-ps.el" (16538
;;;;;;  26944))
;;; Generated autoloads from mhc/emacs/mhc-ps.el

(autoload 'mhc-ps "mhc-ps" "\
*Create PostScript calendar with selected method.

\(fn &optional ARG)" t nil)

(autoload 'mhc-ps-preview "mhc-ps" "\
*Preview PostScript calendar.

\(fn YEAR MONTH &optional CATEGORY-PREDICATE)" t nil)

(autoload 'mhc-ps-print "mhc-ps" "\
*Print PostScript calendar.

\(fn YEAR MONTH &optional CATEGORY-PREDICATE)" t nil)

(autoload 'mhc-ps-save "mhc-ps" "\
*Save PostScript calendar.

\(fn YEAR MONTH FILE &optional CATEGORY-PREDICATE)" t nil)

(autoload 'mhc-ps-insert-buffer "mhc-ps" "\
*Insert PostScript calendar.

\(fn YEAR MONTH BUFFER &optional CATEGORY-PREDICATE)" t nil)

;;;***

;;;### (autoloads (mhc-wl-setup) "mhc-wl" "mhc/emacs/mhc-wl.el" (18364
;;;;;;  61487))
;;; Generated autoloads from mhc/emacs/mhc-wl.el

(autoload 'mhc-wl-setup "mhc-wl" "\
Not documented

\(fn)" nil nil)

;;;***

;;;### (autoloads (paren-backward-sexp paren-forward-sexp paren-toggle-open-paren-context
;;;;;;  paren-toggle-matching-quoted-paren paren-toggle-matching-paired-delimiter
;;;;;;  paren-deactivate paren-activate) "mic-paren" "mic-paren.el"
;;;;;;  (19397 47224))
;;; Generated autoloads from mic-paren.el

(autoload 'paren-activate "mic-paren" "\
Activate mic-paren parenthesis highlighting.
Note: This also deactivates the paren.el
and stig-paren.el packages if they are active!

The following options are available via the customize-feature:
  `paren-priority'
  `paren-overlay-priority'
  `paren-sexp-mode'
  `paren-highlight-at-point'
  `paren-highlight-offscreen'
  `paren-display-message'
  `paren-message-linefeed-display'
  `paren-message-no-match'
  `paren-message-show-linenumber'
  `paren-message-truncate-lines'
  `paren-ding-unmatched'
  `paren-delay'
  `paren-dont-touch-blink'
  `paren-match-face'
  `paren-mismatch-face'
  `paren-no-match-face'
  `paren-bind-modified-sexp-functions'

The following options are settable via toggling functions (look at the
documentation of these options for the names of these functions):
  `paren-match-quoted-paren'
  `paren-match-paired-delimiter'
  `paren-open-paren-context-backward'

\(fn)" t nil)

(autoload 'paren-deactivate "mic-paren" "\
Deactivate mic-paren parenthesis highlighting.

\(fn)" t nil)

(autoload 'paren-toggle-matching-paired-delimiter "mic-paren" "\
Toggle matching paired delimiter.
Force on with positive ARG.  Use this in mode hooks to activate or
deactivate paired delimiter matching.  If optional second argument
NO-MESSAGE is non-nil then don't display a message about the
current activation state of the paired-delimiter-matching feature.

\(fn ARG &optional NO-MESSAGE)" t nil)

(autoload 'paren-toggle-matching-quoted-paren "mic-paren" "\
Toggle matching quoted parens.
Force on with positive ARG.  Use this in mode hooks to activate or
deactivate quoted paren matching.  If optional second argument
NO-MESSAGE is non-nil then don't display a message about the
current activation state of the quoted-paren-matching feature.

\(fn ARG &optional NO-MESSAGE)" t nil)

(autoload 'paren-toggle-open-paren-context "mic-paren" "\
Toggle whether or not to determine context of the matching open-paren.
Force backward context with positive ARG.  Use this in mode-hooks.
See `paren-open-paren-context-backward'.

\(fn ARG)" t nil)

(autoload 'paren-forward-sexp "mic-paren" "\
Act like `forward-sexp' but also handle quoted parens.
See `paren-match-quoted-paren'.

\(fn &optional ARG)" t nil)

(autoload 'paren-backward-sexp "mic-paren" "\
Act like `backward-sexp' but also match quoted parens.
See `paren-match-quoted-paren'.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (mime-w3m-preview-text/html) "mime-w3m" "emacs-w3m/mime-w3m.el"
;;;;;;  (18964 36505))
;;; Generated autoloads from emacs-w3m/mime-w3m.el

(autoload 'mime-w3m-preview-text/html "mime-w3m" "\
Not documented

\(fn ENTITY SITUATION)" nil nil)

;;;***

;;;### (autoloads (mode-line-reminder-duration) "misc-fns" "misc-fns.el"
;;;;;;  (19816 7986))
;;; Generated autoloads from misc-fns.el

(defvar mode-line-reminder-duration 10 "\
*Maximum number of seconds to display a reminder in the mode-line.")

(custom-autoload 'mode-line-reminder-duration "misc-fns" t)

;;;***

;;;### (autoloads (mode-compile-kill mode-compile mode-compile-submit-bug-report
;;;;;;  emacs-lisp-byte-compile-dir-interactive-p mode-compile-reading-time
;;;;;;  mode-compile-expert-p mode-compile-after-kill-hook mode-compile-before-kill-hook
;;;;;;  mode-compile-after-compile-hook mode-compile-before-compile-hook
;;;;;;  mode-compile-other-frame-p mode-compile-never-edit-command-p
;;;;;;  mode-compile-always-save-buffer-p mode-compile-save-all-p
;;;;;;  mode-compile-ignore-makerule-regexp mode-compile-prefered-default-makerule
;;;;;;  mode-compile-make-options mode-compile-ignore-makefile-backups
;;;;;;  mode-compile-make-program) "mode-compile" "mode-compile.el"
;;;;;;  (19160 27200))
;;; Generated autoloads from mode-compile.el

(defvar mode-compile-make-program "make" "\
*The `make' program used to process makefiles.

If you have GNU make installed with name \"gmake\" use it.")

(custom-autoload 'mode-compile-make-program "mode-compile" t)

(defvar mode-compile-ignore-makefile-backups t "\
*Tell mode compile to ignore makefiles backup files when selecting the Makefile to use.")

(custom-autoload 'mode-compile-ignore-makefile-backups "mode-compile" t)

(defvar mode-compile-default-make-options "-k" "\
Default options to give to `make'.")

(defvar mode-compile-make-options (eval mode-compile-default-make-options) "\
*Options to give to `make'.
This could be any form evaluating to a string.

Some people asked me a way to modify the make options everytime a
compilation command is launched, do that:
 (defun my-mode-compile-ask-make-options()
   \"*Hook called by mode-compile, asking for make options.\"
   (interactive)
   (read-string \"Make options: \"
                mode-compile-default-make-options))
 (setq mode-compile-make-options
           'my-mode-compile-ask-make-options)")

(custom-autoload 'mode-compile-make-options "mode-compile" t)

(defvar mode-compile-prefered-default-makerule 'none "\
*Default makerule you would like to see in minibuffer as a default choice
when selecting the make rule to build.

Possible values are:
'none    -- let mode-compile deciding for you.
'all     -- try hard to show you the \"all\" rule.
'default -- try hard to show you the \"default\" rule.
'file    -- try to show you the name of the file which will be
            result of compilation.
The 'none action is taken as default is something fail.")

(custom-autoload 'mode-compile-prefered-default-makerule "mode-compile" t)

(defvar mode-compile-ignore-makerule-regexp nil "\
*Makefile rules which must be ignored when building completion list.

For example if you want to remove all `files rules' set
it to: \"\\\\.\\\\([aoc]\\\\|s[ao][.0-9]*\\\\)\". ")

(custom-autoload 'mode-compile-ignore-makerule-regexp "mode-compile" t)

(defvar mode-compile-save-all-p nil "\
*Non-nil means save ALL the modified buffers without asking
before launching compilation command.")

(custom-autoload 'mode-compile-save-all-p "mode-compile" t)

(defvar mode-compile-always-save-buffer-p nil "\
*Non-nil means save the current buffer without asking
before launching compilation command.")

(custom-autoload 'mode-compile-always-save-buffer-p "mode-compile" t)

(defvar mode-compile-never-edit-command-p nil "\
*Non-nil means never ask to user to edit the compile command.")

(custom-autoload 'mode-compile-never-edit-command-p "mode-compile" t)

(defvar mode-compile-other-frame-p nil "\
*Non-nil means compile in another frame.

A new Emacs FRAME is created and the compilation command is executed
in this other frame.  To specify the frame parameters see also
variable `mode-compile-frame-parameters-alist'.")

(custom-autoload 'mode-compile-other-frame-p "mode-compile" t)

(defvar mode-compile-before-compile-hook nil "\
Hook to be run before compile command is executed
when `mode-compile' is invoked.")

(custom-autoload 'mode-compile-before-compile-hook "mode-compile" t)

(defvar mode-compile-after-compile-hook nil "\
Hook to be run after compile command is executed
when `mode-compile' is invoked.")

(custom-autoload 'mode-compile-after-compile-hook "mode-compile" t)

(defvar mode-compile-before-kill-hook nil "\
Hook to be run before killing compile command is executed
when `mode-compile-kill' is invoked.")

(custom-autoload 'mode-compile-before-kill-hook "mode-compile" t)

(defvar mode-compile-after-kill-hook nil "\
Hook to be run after killing compile command is executed
when `mode-compile-kill' is invoked.")

(custom-autoload 'mode-compile-after-kill-hook "mode-compile" t)

(defvar mode-compile-choosen-compiler nil "\
*Global variable containing the name of the compiler
which will be used for compiling without makefile.

 Could be used in combination with
 (cc|c++|ada|f77)-default-compiler-options
to automaticaly choose the compiler specific options.

example:
 (defun my-compiler-get-options()
   (cond
    ((string= mode-compile-choosen-compiler \"gcc\")
      \"-Wall -pedantic-errors\")
    ((string= mode-compile-choosen-compiler \"cc\")
      \"cc options whatever they are...\")
    (t
     (message \"Don't know this compiler: %s\" mode-compile-choosen-compiler)
     (read-string
      (format \"Options for %s compiler: \" mode-compile-choosen-compiler)))))

  (setq cc-default-compiler-options 'my-compiler-get-options)")

(defvar mode-compile-expert-p nil "\
*Non nil means `mode-compile' will not speaks too much.

See also variable variable mode-compile-reading-time.")

(custom-autoload 'mode-compile-expert-p "mode-compile" t)

(defvar mode-compile-reading-time 1 "\
*Seconds to wait in verbose mode after printing a message.

In verbose mode mode-compile print too much messages that it is
allmost impossible to read them. Just setting this delay leave you the
time to read all the messages. If you don't want any delay set it to
`0'.

See also function sit-for.")

(custom-autoload 'mode-compile-reading-time "mode-compile" t)

(defvar emacs-lisp-byte-compile-dir-interactive-p t "\
*Non-nil means when byte-compiling a directory ask for each file
needing to be recompiled or not.")

(custom-autoload 'emacs-lisp-byte-compile-dir-interactive-p "mode-compile" t)

(defconst mode-compile-version "2.28" "\
Current version of mode-compile package.

mode-compile.el,v 2.28 2003/04/01 13:52:47 boubaker Exp
Please send bugs-fixes/contributions/comments to boubaker@cena.fr")

(autoload 'mode-compile-submit-bug-report "mode-compile" "\
*Submit via mail a bug report on mode-compile v2.27.

\(fn)" t nil)

(autoload 'mode-compile "mode-compile" "\
*Compile the file in the current buffer with a dynamically built command.

The command is built according to the current major mode the function
was invoked from.

Running this command preceded by universal-argument (\\[universal-argument])
allows remote compilation, the user is prompted for a host name to run the
compilation command on.

Currently know how to compile in:
 `c-mode' ,              -- function cc-compile.
 `java-mode' ,           -- function java-compile.
 `c++-mode',             -- function c++-compile.
 `ada-mode',             -- function ada-compile.
 `fortran-mode',         -- function f77-compile.
 `emacs-lisp-mode'       -- function elisp-compile.
 `lisp-interaction-mode' -- function elisp-compile.
 `makefile-mode'         -- function makefile-compile.
 `dired-mode'            -- function dired-compile.
 `sh-mode'               -- function sh-compile.
 `csh-mode'              -- function csh-compile.
 `zsh-mode'              -- function zsh-compile.
 `perl-mode'             -- function perl-compile.
 `cperl-mode'            -- function perl-compile.
 `tcl-mode'              -- function tcl-compile.
 `python-mode'           -- function python-compile.
 `ruby-mode'             -- function ruby-compile.
 `fundamental-mode'      -- function guess-compile.
 `text-mode'             -- function guess-compile.
 `indented-text-mode'    -- function guess-compile.
 `compilation-mode'      -- function default-compile.
 The function `guess-compile' is called when mode is unknown.

The variable `mode-compile-modes-alist' contain description of known
modes.  The hooks variables `mode-compile-before-compile-hook' and
`mode-compile-after-compile-hook' are run just before and after
invoking the compile command of the mode.

Use the command `mode-compile-kill' (\\[mode-compile-kill]) to abort a
running compilation.

Bound on \\[mode-compile].

\(fn &optional REMOTE-HOST)" t nil)

(autoload 'mode-compile-kill "mode-compile" "\
*Kill the running compilation launched by `mode-compile' (\\[mode-compile]) command.

The compilation command is killed according to the current major mode
the function was invoked from.

Currently know how to kill compilations from:
 `c-mode' ,              -- function kill-compilation.
 `java-mode' ,           -- function kill-compilation.
 `c++-mode' ,            -- function kill-compilation.
 `ada-mode' ,            -- function kill-compilation.
 `fortran-mode' ,        -- function kill-compilation.
 `emacs-lisp-mode'       -- function keyboard-quit.
 `lisp-interaction-mode' -- function keyboard-quit.
 `makefile-mode'         -- function kill-compilation.
 `dired-mode'            -- function kill-compilation.
 `sh-mode'               -- function kill-compilation.
 `csh-mode'              -- function kill-compilation.
 `zsh-mode'              -- function kill-compilation.
 `perl-mode'             -- function kill-compilation.
 `cperl-mode'            -- function kill-compilation.
 `tcl-mode'              -- function kill-compilation.
 `python-mode'           -- function kill-compilation.
 `ruby-mode'             -- function kill-compilation.
 `fundamental-mode'      -- Bound dynamically.
 `text-mode'             -- Bound dynamically.
 `indented-text-mode'    -- Bound dynamically.
 `compilation-mode'      -- function kill-compilation.

The variable `mode-compile-modes-alist' contain description of ALL
known modes.  The hooks variables `mode-compile-before-kill-hook' and
`mode-compile-after-kill-hook' are run just before and after invoking
the kill compile command of the mode.

Bound on \\[mode-compile-kill].

\(fn)" t nil)

;;;***

;;;### (autoloads (mode-local-read-function) "mode-local" "cedet/common/mode-local.el"
;;;;;;  (19390 36251))
;;; Generated autoloads from cedet/common/mode-local.el

(autoload 'mode-local-read-function "mode-local" "\
Interactively read in the name of a mode-local function.
PROMPT, INITIAL, HIST, and DEFAULT are the same as for `completing-read'.

\(fn PROMPT &optional INITIAL HIST DEFAULT)" nil nil)

;;;***

;;;### (autoloads (bbdb/send-hook bbdb/send-auto-notes-hook bbdb/send-ignore-some-messages-hook
;;;;;;  bbdb/send-ignore-most-messages-hook) "moy-bbdb" "moy-bbdb.el"
;;;;;;  (19406 54950))
;;; Generated autoloads from moy-bbdb.el

(autoload 'bbdb/send-ignore-most-messages-hook "moy-bbdb" "\
For use as the value of `bbdb/send-auto-create-p'.
This will automatically create BBDB entries for messages which match
the bbdb/send-ignore-most-messages-alist (which see) and *no* others.

\(fn &optional INVERT-SENSE)" nil nil)

(autoload 'bbdb/send-ignore-some-messages-hook "moy-bbdb" "\
For use as a `bbdb/send-auto-create-hook'.
This will automatically create BBDB entries for messages which do *not*
match the `bbdb/send-ignore-some-messages-alist' (which see).

\(fn)" nil nil)

(autoload 'bbdb/send-auto-notes-hook "moy-bbdb" "\
For use as a `bbdb/send-notice-hook'.  This might automatically add
some text to  the notes field of the BBDB  record corresponding to the
current record  based on the header  of the current  message.  See the
documentation  for   the  variables  `bbdb/send-auto-notes-alist'  and
`bbdb/send-auto-notes-ignore'.

\(fn RECORD)" nil nil)

(autoload 'bbdb/send-hook "moy-bbdb" "\
Parse headers of outgoing message, insert the addresses of the
  recipients one by one into BBDB if they do not exist already

\(fn)" t nil)

;;;***

;;;### (autoloads (inferior-moz-mode moz-minor-mode) "moz" "moz.el"
;;;;;;  (19824 1707))
;;; Generated autoloads from moz.el

(autoload 'moz-minor-mode "moz" "\
MozRepl minor mode for interaction with Firefox.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When this minor mode is enabled, some commands become available
to send current code area (as understood by c-mark-function) or
region or buffer to an inferior MozRepl process (which will be
started as needed).

The following keys are bound in this minor mode:

\\{moz-minor-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'inferior-moz-mode "moz" "\
Major mode for interacting with Firefox via MozRepl.

\(fn)" t nil)

;;;***

;;;### (autoloads (mu-bbdb-get-prefix-register-verbose-method mu-bbdb-get-prefix-register-method
;;;;;;  mu-bbdb-get-prefix-method) "mu-bbdb" "mu-cite-8.1/mu-bbdb.el"
;;;;;;  (14246 32523))
;;; Generated autoloads from mu-cite-8.1/mu-bbdb.el

(autoload 'mu-bbdb-get-prefix-method "mu-bbdb" "\
A mu-cite method to return a prefix from BBDB or \">\".
If an `attribution' value is found in BBDB, the value is returned.
Otherwise \">\" is returned.

Notice that please use (mu-cite-get-value 'bbdb-prefix)
instead of call the function directly.

\(fn)" nil nil)

(autoload 'mu-bbdb-get-prefix-register-method "mu-bbdb" "\
A mu-cite method to return a prefix from BBDB or register it.
If an `attribution' value is found in BBDB, the value is returned.
Otherwise the function requests a prefix from a user.  The prefix will
be registered to BBDB if the user wants it.

Notice that please use (mu-cite-get-value 'bbdb-prefix-register)
instead of call the function directly.

\(fn)" nil nil)

(autoload 'mu-bbdb-get-prefix-register-verbose-method "mu-bbdb" "\
A mu-cite method to return a prefix using BBDB.

In this method, a user must specify a prefix unconditionally.  If an
`attribution' value is found in BBDB, the value is used as a initial
value to input the prefix.  The prefix will be registered to BBDB if
the user wants it.

Notice that please use (mu-cite-get-value 'bbdb-prefix-register-verbose)
instead of call the function directly.

\(fn)" nil nil)

;;;***

;;;### (autoloads (compress-cited-prefix fill-cited-region mu-cite-original)
;;;;;;  "mu-cite" "mu-cite-8.1/mu-cite.el" (19408 6164))
;;; Generated autoloads from mu-cite-8.1/mu-cite.el

(autoload 'mu-cite-original "mu-cite" "\
Citing filter function.
This is callable from the various mail and news readers' reply
function according to the agreed upon standard.

\(fn)" t nil)

(autoload 'fill-cited-region "mu-cite" "\
Fill each of the paragraphs in the region as a cited text.

\(fn BEG END)" t nil)

(autoload 'compress-cited-prefix "mu-cite" "\
Compress nested cited prefixes.

\(fn)" t nil)

;;;***

;;;### (autoloads (mu-cite-get-prefix-register-verbose-method mu-cite-get-prefix-register-method
;;;;;;  mu-cite-get-prefix-method) "mu-register" "mu-cite-8.1/mu-register.el"
;;;;;;  (14424 29165))
;;; Generated autoloads from mu-cite-8.1/mu-register.el

(autoload 'mu-cite-get-prefix-method "mu-register" "\
Not documented

\(fn)" nil nil)

(autoload 'mu-cite-get-prefix-register-method "mu-register" "\
Not documented

\(fn)" nil nil)

(autoload 'mu-cite-get-prefix-register-verbose-method "mu-register" "\
Not documented

\(fn)" nil nil)

;;;***

;;;### (autoloads (multi-term) "multi-term" "multi-term.el" (19801
;;;;;;  36994))
;;; Generated autoloads from multi-term.el

(autoload 'multi-term "multi-term" "\
Create new term buffer.
Will prompt you shell name when you type `C-u' before this command.

\(fn)" t nil)

;;;***

;;;### (autoloads (mwe:open-command-log-buffer mwe:log-keyboard-commands)
;;;;;;  "mwe-log-commands" "mwe-log-commands.el" (19828 48130))
;;; Generated autoloads from mwe-log-commands.el

(autoload 'mwe:log-keyboard-commands "mwe-log-commands" "\
Enables keyboard command logging for the current buffer.
If optional ARG is nil, logging is turned off.

\(fn &optional ARG)" t nil)

(autoload 'mwe:open-command-log-buffer "mwe-log-commands" "\
Opens (and creates, if non-existant) a buffer used for logging keyboard commands.
If ARG is Non-nil, the existing command log buffer is cleared.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (newsticker-start newsticker-running-p) "newst-backend"
;;;;;;  "newsticker-1.99/newst-backend.el" (18675 38706))
;;; Generated autoloads from newsticker-1.99/newst-backend.el

(autoload 'newsticker-running-p "newst-backend" "\
Check whether newsticker is running.
Return t if newsticker is running, nil otherwise.  Newsticker is
considered to be running if the newsticker timer list is not empty.

\(fn)" nil nil)

(autoload 'newsticker-start "newst-backend" "\
Start the newsticker.
Start the timers for display and retrieval.  If the newsticker, i.e. the
timers, are running already a warning message is printed unless
DO-NOT-COMPLAIN-IF-RUNNING is not nil.
Run `newsticker-start-hook' if newsticker was not running already.

\(fn &optional DO-NOT-COMPLAIN-IF-RUNNING)" t nil)

;;;***

;;;### (autoloads (newsticker-plainview) "newst-plainview" "newsticker-1.99/newst-plainview.el"
;;;;;;  (18675 38706))
;;; Generated autoloads from newsticker-1.99/newst-plainview.el

(autoload 'newsticker-plainview "newst-plainview" "\
Start newsticker plainview.

\(fn)" t nil)

;;;***

;;;### (autoloads (newsticker-show-news) "newst-reader" "newsticker-1.99/newst-reader.el"
;;;;;;  (18675 38706))
;;; Generated autoloads from newsticker-1.99/newst-reader.el

(autoload 'newsticker-show-news "newst-reader" "\
Start reading news.  You may want to bind this to a key.

\(fn)" t nil)

;;;***

;;;### (autoloads (newsticker-start-ticker newsticker-ticker-running-p)
;;;;;;  "newst-ticker" "newsticker-1.99/newst-ticker.el" (18675 38706))
;;; Generated autoloads from newsticker-1.99/newst-ticker.el

(autoload 'newsticker-ticker-running-p "newst-ticker" "\
Check whether newsticker's actual ticker is running.
Return t if ticker is running, nil otherwise.  Newsticker is
considered to be running if the newsticker timer list is not
empty.

\(fn)" nil nil)

(autoload 'newsticker-start-ticker "newst-ticker" "\
Start newsticker's ticker (but not the news retrieval).
Start display timer for the actual ticker if wanted and not
running already.

\(fn)" t nil)

;;;***

;;;### (autoloads (newsticker-treeview) "newst-treeview" "newsticker-1.99/newst-treeview.el"
;;;;;;  (18675 38706))
;;; Generated autoloads from newsticker-1.99/newst-treeview.el

(autoload 'newsticker-treeview "newst-treeview" "\
Start newsticker treeview.

\(fn)" t nil)

;;;***

;;;### (autoloads (gnus-group-make-shimbun-groups gnus-group-make-shimbun-group
;;;;;;  gnus-summary-refer-shimbun-article) "nnshimbun" "emacs-w3m/shimbun/nnshimbun.el"
;;;;;;  (19101 52639))
;;; Generated autoloads from emacs-w3m/shimbun/nnshimbun.el

(autoload 'gnus-summary-refer-shimbun-article "nnshimbun" "\
Show a shimbun article pointed to by the given URL.

\(fn URL)" t nil)

(autoload 'gnus-group-make-shimbun-group "nnshimbun" "\
Create a new nnshimbun group.
The user will be prompted for a SERVER name and a GROUP name.  When
this command is called with a prefix argument, it makes an ephemeral
shimbun group.

\(fn SERVER GROUP &optional EPHEMERAL)" t nil)

(autoload 'gnus-group-make-shimbun-groups "nnshimbun" "\
Create all nnshimbun groups prepared for SERVER.

\(fn SERVER)" t nil)

;;;***

;;;### (autoloads (octet-mime-setup mime-view-octet mime-preview-octet
;;;;;;  octet-find-file octet-buffer) "octet" "emacs-w3m/octet.el"
;;;;;;  (17133 57129))
;;; Generated autoloads from emacs-w3m/octet.el

(autoload 'octet-buffer "octet" "\
View octet-stream content according to `octet-type-filter-alist'.
Optional NAME is the filename.
If optional CONTENT-TYPE is specified, it is used for type guess.

\(fn &optional NAME CONTENT-TYPE)" t nil)

(autoload 'octet-find-file "octet" "\
Find FILE with octet-stream decoding.

\(fn FILE)" t nil)

(autoload 'mime-preview-octet "octet" "\
A method for mime-view to preview octet message.

\(fn ENTITY SITUATION)" nil nil)

(autoload 'mime-view-octet "octet" "\
A method for mime-view to display octet message.

\(fn ENTITY SITUATION)" nil nil)

(autoload 'octet-mime-setup "octet" "\
Octet setting for MIME module.

\(fn)" nil nil)

;;;***

;;;### (autoloads (openwith-mode) "openwith" "openwith.el" (18465
;;;;;;  38760))
;;; Generated autoloads from openwith.el

(defvar openwith-mode nil "\
Non-nil if Openwith mode is enabled.
See the command `openwith-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `openwith-mode'.")

(custom-autoload 'openwith-mode "openwith" nil)

(autoload 'openwith-mode "openwith" "\
Automatically open files with external programs.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (org-fireforg-registry-update org-fireforg-registry-insinuate
;;;;;;  org-fireforg-registry-initialize) "org-fireforg" "org-fireforg.el"
;;;;;;  (19810 18344))
;;; Generated autoloads from org-fireforg.el

(autoload 'org-fireforg-registry-initialize "org-fireforg" "\
Initialize `org-fireforg-registry-alist'. 
If FROM-SCRATCH is non-nil or the registry does not exist yet,
create a new registry from scratch and eval it. If the registry
exists, eval `org-fireforg-registry-file' and make it the new value for
`org-fireforg-registry-alist'.

\(fn &optional FROM-SCRATCH)" t nil)

(autoload 'org-fireforg-registry-insinuate "org-fireforg" "\
Call `org-fireforg-registry-update' after saving in Org-mode.
Use with caution.  This could slow down things a bit.

\(fn)" t nil)

(autoload 'org-fireforg-registry-update "org-fireforg" "\
Update the registry for the current Org file, if it is in org-fireforg-registry-file-set.

\(fn)" t nil)

;;;***

;;;### (autoloads (paredit-mode) "paredit" "paredit.el" (19826 17897))
;;; Generated autoloads from paredit.el

(autoload 'paredit-mode "paredit" "\
Minor mode for pseudo-structurally editing Lisp code.
With a prefix argument, enable Paredit Mode even if there are
  imbalanced parentheses in the buffer.
Paredit behaves badly if parentheses are imbalanced, so exercise
  caution when forcing Paredit Mode to be enabled, and consider
  fixing imbalanced parentheses instead.
\\<paredit-mode-map>

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (cogre-picture-insert-rectangle) "picture-hack"
;;;;;;  "cedet/cogre/picture-hack.el" (18897 57890))
;;; Generated autoloads from cedet/cogre/picture-hack.el

(autoload 'cogre-picture-insert-rectangle "picture-hack" "\
Overlay RECTANGLE with upper left corner at point.
Leaves the region surrounding the rectangle.

\(fn RECTANGLE)" nil nil)

;;;***

;;;### (autoloads (refresh-pretty-control-l pp^L-^L-string Pretty-Control-L)
;;;;;;  "pp-c-l" "pp-c-l.el" (19822 7308))
;;; Generated autoloads from pp-c-l.el

(let ((loads (get 'Pretty-Control-L 'custom-loads))) (if (member '"pp-c-l" loads) nil (put 'Pretty-Control-L 'custom-loads (cons '"pp-c-l" loads))))

(defface pp^L-highlight (if (> emacs-major-version 21) '((((type x w32 mac graphic) (class color)) (:box (:line-width 3 :style pressed-button))) (t (:inverse-video t))) '((((type x w32 mac graphic) (class color)) (:foreground "Blue" :background "DarkSeaGreen1")) (t (:inverse-video t)))) "\
*Face used to highlight `pp^L-^L-vector'." :group (quote Pretty-Control-L) :group (quote faces))

(defvar pp^L-^L-string "          Section (Printable Page)          " "\
*Highlighted string displayed in place of each Control-l (^L) character.
If `pp^L-^L-string-function' is non-nil, then the string that function
returns is used instead of `pp^L-^L-string'.")

(custom-autoload 'pp^L-^L-string "pp-c-l" t)

(defalias 'pp^l 'pretty-control-l-mode)

(autoload 'refresh-pretty-control-l "pp-c-l" "\
Reinitialize `pretty-control-l-mode', if on, to update the display.

\(fn)" t nil)

;;;***

;;;### (autoloads (pprint-function pprint pprint-to-string) "pprint"
;;;;;;  "cedet/common/pprint.el" (17213 39693))
;;; Generated autoloads from cedet/common/pprint.el

(autoload 'pprint-to-string "pprint" "\
Return a string containing the pretty-printed representation of OBJECT.
OBJECT can be any Lisp object.  Quoting characters are used as needed
to make output that `read' can handle, whenever this is possible.  The
pretty printer try as much as possible to limit the length of lines to
given WIDTH.  WIDTH value defaults to `fill-column'.

\(fn OBJECT &optional WIDTH)" nil nil)

(autoload 'pprint "pprint" "\
Output the pretty-printed representation of OBJECT, any Lisp object.
Quoting characters are printed as needed to make output that `read'
can handle, whenever this is possible.  Output stream is STREAM, or
value of `standard-output' (which see).  The pretty printer try as
much as possible to limit the length of lines to given WIDTH.  WIDTH
value defaults to `fill-column'.

\(fn OBJECT &optional STREAM WIDTH)" nil nil)

(autoload 'pprint-function "pprint" "\
See a pretty-printed representation of FUNCTION-NAME.

\(fn FUNCTION-NAME)" t nil)

;;;***

;;;### (autoloads nil "ps-ccrypt" "ps-ccrypt.el" (19826 17458))
;;; Generated autoloads from ps-ccrypt.el
(defun auto-encryption-mode (&optional arg)
 "\
Toggle automatic file encryption and decryption.
With prefix argument ARG, turn auto encryption on if positive, else off.
Returns the new status of auto encryption (non-nil means on)."
 (interactive "P")
 (if (not (fboundp 'ps-ccrypt-installed-p))
     (progn
       (require 'ps-ccrypt)
       ;; That turned the mode on, so make it initially off.
       (toggle-auto-encryption)))
 (toggle-auto-encryption arg t))

;;;***

;;;### (autoloads (svn-status svn-checkout) "psvn" "psvn.el" (19826
;;;;;;  17373))
;;; Generated autoloads from psvn.el

(autoload 'svn-checkout "psvn" "\
Run svn checkout REPOS-URL PATH.

\(fn REPOS-URL PATH)" t nil)
 (defalias 'svn-examine 'svn-status)

(autoload 'svn-status "psvn" "\
Examine the status of Subversion working copy in directory DIR.
If ARG is -, allow editing of the parameters. One could add -N to
run svn status non recursively to make it faster.
For every other non nil ARG pass the -u argument to `svn status', which
asks svn to connect to the repository and check to see if there are updates
there.

If there is no .svn directory, examine if there is CVS and run
`cvs-examine'. Otherwise ask if to run `dired'.

\(fn DIR &optional ARG)" t nil)

;;;***

;;;### (autoloads (pulse-line-hook-function pulse-toggle-integration-advice
;;;;;;  pulse-momentary-highlight-region pulse-momentary-highlight-one-line
;;;;;;  pulse-momentary-highlight-overlay pulse-test pulse) "pulse"
;;;;;;  "cedet/common/pulse.el" (19539 31151))
;;; Generated autoloads from cedet/common/pulse.el

(autoload 'pulse "pulse" "\
Pulse the colors on our highlight face.
If optional FACE is provide, reset the face to FACE color,
instead of `pulse-highlight-start-face'.
Be sure to call `pulse-reset-face' after calling pulse.

\(fn &optional FACE)" nil nil)

(autoload 'pulse-test "pulse" "\
Test the lightening function for pulsing a line.
When optional NO-ERROR Don't throw an error if we can't run tests.

\(fn &optional NO-ERROR)" t nil)

(autoload 'pulse-momentary-highlight-overlay "pulse" "\
Pulse the overlay O, unhighlighting before next command.
Optional argument FACE specifies the fact to do the highlighting.

\(fn O &optional FACE)" nil nil)

(autoload 'pulse-momentary-highlight-one-line "pulse" "\
Highlight the line around POINT, unhighlighting before next command.
Optional argument FACE specifies the face to do the highlighting.

\(fn POINT &optional FACE)" nil nil)

(autoload 'pulse-momentary-highlight-region "pulse" "\
Highlight between START and END, unhighlighting before next command.
Optional argument FACE specifies the fact to do the highlighting.

\(fn START END &optional FACE)" nil nil)

(autoload 'pulse-toggle-integration-advice "pulse" "\
Toggle activation of advised functions that will now pulse.
Wint no ARG, toggle the pulse advice.
With a negative ARG, disable pulse advice.
With a positive ARG, enable pulse advice.
Currently advised functions include:
  `goto-line'
  `exchange-point-and-mark'
  `find-tag'
  `tags-search'
  `tags-loop-continue'
  `pop-tag-mark'
  `imenu-default-goto-function'
Pulsing via `pulse-line-hook-function' has also been added to
the following hook:
  `next-error-hook'

\(fn ARG)" t nil)

(autoload 'pulse-line-hook-function "pulse" "\
Function used in hooks to pulse the current line.
Only pulses the line if `pulse-command-advice-flag' is non-nil.

\(fn)" nil nil)

;;;***

;;;### (autoloads (pwsafe-add-entry pwsafe) "pwsafe" "pwsafe.el"
;;;;;;  (19826 16452))
;;; Generated autoloads from pwsafe.el

(autoload 'pwsafe "pwsafe" "\
Major mode to interact with the command line password safe pwsafe.
Queries the passwords from the password safe and displays them in the buffer *pwsafe-list*.
The following keys are defined:
\\{pwsafe-list-mode-map}

\(fn FORCE)" t nil)

(autoload 'pwsafe-add-entry "pwsafe" "\
Not documented

\(fn NAME)" t nil)

;;;***

;;;### (autoloads (py-shell python-mode) "python-mode" "python-mode/python-mode.el"
;;;;;;  (19753 44085))
;;; Generated autoloads from python-mode/python-mode.el

(autoload 'python-mode "python-mode" "\
Major mode for editing Python files.
To submit a problem report, enter `\\[py-submit-bug-report]' from a
`python-mode' buffer.  Do `\\[py-describe-mode]' for detailed
documentation.  To see what version of `python-mode' you are running,
enter `\\[py-version]'.

This mode knows about Python indentation, tokens, comments and
continuation lines.  Paragraphs are separated by blank lines only.

COMMANDS
\\{py-mode-map}
VARIABLES

py-indent-offset		indentation increment
py-block-comment-prefix		comment string used by `comment-region'
py-python-command		shell command to invoke Python interpreter
py-temp-directory		directory used for temp files (if needed)
py-beep-if-tab-change		ring the bell if `tab-width' is changed

\(fn)" t nil)

(let ((modes '(("jython" . jython-mode) ("python" . python-mode)))) (while modes (when (not (assoc (car modes) interpreter-mode-alist)) (push (car modes) interpreter-mode-alist)) (setq modes (cdr modes))))

(when (not (or (rassq 'python-mode auto-mode-alist) (rassq 'jython-mode auto-mode-alist))) (push '("\\.py$" . python-mode) auto-mode-alist))

(autoload 'py-shell "python-mode" "\
Start an interactive Python interpreter in another window.
This is like Shell mode, except that Python is running in the window
instead of a shell.  See the `Interactive Shell' and `Shell Mode'
sections of the Emacs manual for details, especially for the key
bindings active in the `*Python*' buffer.

With optional \\[universal-argument], the user is prompted for the
flags to pass to the Python interpreter.  This has no effect when this
command is used to switch to an existing process, only when a new
process is started.  If you use this, you will probably want to ensure
that the current arguments are retained (they will be included in the
prompt).  This argument is ignored when this function is called
programmatically, or when running in Emacs 19.34 or older.

Note: You can toggle between using the CPython interpreter and the
Jython interpreter by hitting \\[py-toggle-shells].  This toggles
buffer local variables which control whether all your subshell
interactions happen to the `*Jython*' or `*Python*' buffers (the
latter is the name used for the CPython buffer).

Warning: Don't use an interactive Python if you change sys.ps1 or
sys.ps2 from their default values, or if you're running code that
prints `>>> ' or `... ' at the start of a line.  `python-mode' can't
distinguish your output from Python's output, and assumes that `>>> '
at the start of a line is a prompt from Python.  Similarly, the Emacs
Shell mode code assumes that both `>>> ' and `... ' at the start of a
line are Python prompts.  Bad things can happen if you fool either
mode.

Warning:  If you do any editing *in* the process buffer *while* the
buffer is accepting output from Python, do NOT attempt to `undo' the
changes.  Some of the output (nowhere near the parts you changed!) may
be lost if you do.  This appears to be an Emacs bug, an unfortunate
interaction between undo and process filters; the same problem exists in
non-Python process buffers using the default (Emacs-supplied) process
filter.

\(fn &optional ARGPROMPT)" t nil)

;;;***

;;;### (autoloads (inferior-qi) "qi-mode" "qi-mode.el" (18760 13748))
;;; Generated autoloads from qi-mode.el

(defvar inferior-qi-filter-regexp "\\`\\s *\\(:\\(\\w\\|\\s_\\)\\)?\\s *\\'" "\
*What not to save on inferior Qi's input history.
Input matching this regexp is not saved on the input history in Inferior Qi
mode.  Default is whitespace followed by 0 or 1 single-letter colon-keyword
\(as in :a, :c, etc.)")

(defvar inferior-qi-program "Qi" "\
*Program name for invoking an inferior Qi with for Inferior Qi mode.")

(defvar inferior-qi-load-command "(load \"%s\")\n" "\
*Format-string for building a Qi expression to load a file.
This format string should use `%s' to substitute a file name
and should result in a Qi expression that will command the inferior Qi
to load that file.  The default works acceptably on most Qis.
The string \"(progn (load \\\"%s\\\" :verbose nil :print t) (values))\\n\"
produces cosmetically superior output for this application,
but it works only in Common Qi.")

(defvar inferior-qi-prompt "^[^> \n]*>+:? *" "\
Regexp to recognise prompts in the Inferior Qi mode.
Defaults to \"^[^> \\n]*>+:? *\", which works pretty good for Lucid, kcl,
and franz.  This variable is used to initialize `comint-prompt-regexp' in the
Inferior Qi buffer.

This variable is only used if the variable
`comint-use-prompt-regexp-instead-of-fields' is non-nil.

More precise choices:
Lucid Common Qi: \"^\\\\(>\\\\|\\\\(->\\\\)+\\\\) *\"
franz: \"^\\\\(->\\\\|<[0-9]*>:\\\\) *\"
kcl: \"^>+ *\"

This is a fine thing to set in your .emacs file.")

(defvar inferior-qi-mode-hook 'nil "\
*Hook for customising Inferior Qi mode.")

(autoload 'inferior-qi "qi-mode" "\
Run an inferior Qi process, input and output via buffer `*inferior-qi*'.
If there is a process already running in `*inferior-qi*', just switch
to that buffer.
With argument, allows you to edit the command line (default is value
of `inferior-qi-program').  Runs the hooks from
`inferior-qi-mode-hook' (after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)

\(fn CMD)" t nil)
 (add-hook 'same-window-buffer-names "*inferior-qi*")

(defalias 'run-qi 'inferior-qi)

;;;***

;;;### (autoloads (quickpeek-help quickpeek-get-focus quickpeek-frame-mode)
;;;;;;  "quickpeek" "cedet/quickpeek/quickpeek.el" (17213 41766))
;;; Generated autoloads from cedet/quickpeek/quickpeek.el

(defalias 'quickpeek 'quickpeek-frame-mode)

(autoload 'quickpeek-frame-mode "quickpeek" "\
Initialize `quickpeek'.
If optional ARG is less than 0, turn off this mode, positive turn on.
If nil, then toggle.

\(fn &optional ARG)" t nil)

(autoload 'quickpeek-get-focus "quickpeek" "\
Change frame focus to or from the `quickpeek' frame.
If the selected frame is not `quickpeek', then `quickpeek' frame is
selected.  If the `quickpeek' frame is active, then select the attached frame.

\(fn)" t nil)

(autoload 'quickpeek-help "quickpeek" "\
Display a quickpeek buffer in a temporary window.
Like `quickpeek', but without an extra frame, timers, or tracking.

\(fn)" t nil)

;;;***

;;;### (autoloads (rm-mouse-drag-region rm-kill-ring-save rm-kill-region
;;;;;;  rm-exchange-point-and-mark rm-set-mark rm-example-picture-mode-bindings)
;;;;;;  "rect-mark" "rect-mark.el" (19826 16220))
;;; Generated autoloads from rect-mark.el
 (define-key ctl-x-map "r\C-@" 'rm-set-mark)
 (define-key ctl-x-map [?r ?\C-\ ] 'rm-set-mark)
 (define-key ctl-x-map "r\C-x" 'rm-exchange-point-and-mark)
 (define-key ctl-x-map "r\C-w" 'rm-kill-region)
 (define-key ctl-x-map "r\M-w" 'rm-kill-ring-save)
 (define-key global-map [S-down-mouse-1] 'rm-mouse-drag-region)

(autoload 'rm-example-picture-mode-bindings "rect-mark" "\
Example rect-mark keyboard and mouse bindings for picture mode.

\(fn)" nil nil)

(autoload 'rm-set-mark "rect-mark" "\
Set mark like `set-mark-command' but anticipates a rectangle.
This arranges for the rectangular region between point and mark
to be highlighted using the same face that is used to highlight
the region in `transient-mark-mode'.  This special state lasts only
until the mark is deactivated, usually by executing a text-modifying
command like \\[kill-rectangle], by inserting text, or by typing \\[keyboard-quit].

With optional argument FORCE, arrange for tabs to be expanded and
for spaces to inserted as necessary to keep the region perfectly
rectangular.  This is the default in `picture-mode'.

\(fn FORCE)" t nil)

(autoload 'rm-exchange-point-and-mark "rect-mark" "\
Like `exchange-point-and-mark' but treats region as a rectangle.
See `rm-set-mark' for more details.

With optional argument FORCE, tabs are expanded and spaces are
inserted as necessary to keep the region perfectly rectangular.
This is the default in `picture-mode'.

\(fn FORCE)" t nil)

(autoload 'rm-kill-region "rect-mark" "\
Like kill-rectangle except the rectangle is also saved in the kill ring.
Since rectangles are not ordinary text, the killed rectangle is saved
in the kill ring as a series of lines, one for each row of the rectangle.
The rectangle is also saved as the killed rectangle so it is available for
insertion with yank-rectangle.

\(fn START END)" t nil)

(autoload 'rm-kill-ring-save "rect-mark" "\
Copies the region like rm-kill-region would but the rectangle isn't killed.

\(fn START END)" t nil)

(autoload 'rm-mouse-drag-region "rect-mark" "\
Highlight a rectangular region of text as the the mouse is dragged over it.
This must be bound to a button-down mouse event.

\(fn START-EVENT)" t nil)

;;;***

;;;### (autoloads (remember-diary-extract-entries remember-clipboard
;;;;;;  remember-other-frame remember) "remember" "remember/remember.el"
;;;;;;  (19785 38401))
;;; Generated autoloads from remember/remember.el

(autoload 'remember "remember" "\
Remember an arbitrary piece of data.
INITIAL is the text to initially place in the *Remember* buffer,
or nil to bring up a blank *Remember* buffer.

With a prefix or a visible region, use the region as INITIAL.

\(fn &optional INITIAL)" t nil)

(autoload 'remember-other-frame "remember" "\
Call `remember' in another frame.

\(fn &optional INITIAL)" t nil)

(autoload 'remember-clipboard "remember" "\
Remember the contents of the current clipboard.
Most useful for remembering things from Netscape or other X Windows
application.

\(fn)" t nil)

(autoload 'remember-diary-extract-entries "remember" "\
Extract diary entries from the region.

\(fn)" nil nil)

;;;***

;;;### (autoloads (remember-bbdb-store-in-mailbox) "remember-bbdb"
;;;;;;  "remember/remember-bbdb.el" (19785 38401))
;;; Generated autoloads from remember/remember-bbdb.el

(autoload 'remember-bbdb-store-in-mailbox "remember-bbdb" "\
Store remember data as if it were incoming mail.
In which case `remember-mailbox' should be the name of the mailbox.
Each piece of psuedo-mail created will have an `X-Todo-Priority'
field, for the purpose of appropriate splitting.

\(fn)" nil nil)

;;;***

;;;### (autoloads (remember-location remember-url) "remember-bibl"
;;;;;;  "remember/remember-bibl.el" (19785 38401))
;;; Generated autoloads from remember/remember-bibl.el

(autoload 'remember-url "remember-bibl" "\
Remember a URL in `bibl-mode' that is being visited with w3.

\(fn)" t nil)

(autoload 'remember-location "remember-bibl" "\
Remember a bookmark location in `bibl-mode'.

\(fn)" t nil)

;;;***

;;;### (autoloads (remember-blosxom) "remember-blosxom" "remember/remember-blosxom.el"
;;;;;;  (19785 38401))
;;; Generated autoloads from remember/remember-blosxom.el

(autoload 'remember-blosxom "remember-blosxom" "\
Remember this text to a blosxom story.
This function can be added to `remember-handler-functions'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (remember-emacs-wiki-journal-add-entry-maybe remember-emacs-wiki-journal-add-entry-auto
;;;;;;  remember-emacs-wiki-journal-add-entry) "remember-emacs-wiki-journal"
;;;;;;  "remember/remember-emacs-wiki-journal.el" (19785 38401))
;;; Generated autoloads from remember/remember-emacs-wiki-journal.el

(autoload 'remember-emacs-wiki-journal-add-entry "remember-emacs-wiki-journal" "\
Prompt for category and heading and add entry.

\(fn)" nil nil)

(autoload 'remember-emacs-wiki-journal-add-entry-auto "remember-emacs-wiki-journal" "\
Add entry where the category is the first word and the heading the
rest of the words on the first line.

\(fn)" nil nil)

(autoload 'remember-emacs-wiki-journal-add-entry-maybe "remember-emacs-wiki-journal" "\
Like `remember-emacs-wiki-journal-add-entry-auto' but only adds
entry if the first line matches `emacs-wiki-journal-category-regexp'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (remember-planner-append) "remember-planner" "remember/remember-planner.el"
;;;;;;  (19785 38401))
;;; Generated autoloads from remember/remember-planner.el

(autoload 'remember-planner-append "remember-planner" "\
Remember this text to PAGE or today's page.
This function can be added to `remember-handler-functions'.

\(fn &optional PAGE)" nil nil)

;;;***

;;;### (autoloads (resume save-current-configuration wipe restore-window-configuration
;;;;;;  current-window-configuration-printable) "revive" "revive.el"
;;;;;;  (19832 54734))
;;; Generated autoloads from revive.el

(autoload 'current-window-configuration-printable "revive" "\
Return the printable current-window-configuration.
This configuration will be stored by restore-window-configuration.
Returned configurations are list of:
'(Screen-Width Screen-Height Edge-List Buffer-List)

Edge-List is a return value of revive:all-window-edges, list of all
window-edges whose first member is always of north west window.

Buffer-List is a list of buffer property list of all windows.  This
property lists are stored in order corresponding to Edge-List.  Buffer
property list is formed as
'((buffer-file-name) (buffer-name) (point) (window-start)).

\(fn)" nil nil)

(autoload 'restore-window-configuration "revive" "\
Restore the window configuration.
Configuration CONFIG should be created by
current-window-configuration-printable.

\(fn CONFIG)" nil nil)

(autoload 'wipe "revive" "\
Wipe Emacs.

\(fn)" t nil)

(autoload 'save-current-configuration "revive" "\
Save current window/buffer configuration into configuration file.

\(fn &optional NUM)" t nil)

(autoload 'resume "revive" "\
Resume window/buffer configuration.
Configuration should be saved by save-current-configuration.

\(fn &optional NUM)" t nil)

;;;***

;;;### (autoloads (rinari-minor-mode rinari-launch) "rinari" "rinari/rinari.el"
;;;;;;  (19809 16085))
;;; Generated autoloads from rinari/rinari.el

(autoload 'rinari-launch "rinari" "\
Run `rinari-minor-mode' if inside of a rails projecct,
otherwise turn `rinari-minor-mode' off if it is on.

\(fn)" t nil)

(defvar rinari-major-modes (if (boundp 'rinari-major-modes) rinari-major-modes (list 'find-file-hook 'mumamo-after-change-major-mode-hook 'dired-mode-hook)) "\
Major Modes from which to launch Rinari.")

(dolist (hook rinari-major-modes) (add-hook hook 'rinari-launch))

(autoload 'rinari-minor-mode "rinari" "\
Enable Rinari minor mode providing Emacs support for working
with the Ruby on Rails framework.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (rinari-merb-minor-mode) "rinari-merb" "rinari/rinari-merb.el"
;;;;;;  (19809 16085))
;;; Generated autoloads from rinari/rinari-merb.el

(autoload 'rinari-merb-minor-mode "rinari-merb" "\
Enable Rinari-Merb minor mode providing Emacs support for working
with the Ruby on Rails framework.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (rpm) "rpm" "cedet/speedbar/rpm.el" (17213 40778))
;;; Generated autoloads from cedet/speedbar/rpm.el

(autoload 'rpm "rpm" "\
Red Hat Package Management in Emacs.

\(fn)" t nil)

;;;***

;;;### (autoloads (ruby-compilation-this-buffer ruby-compilation-cap
;;;;;;  ruby-compilation-rake ruby-compilation-run pcomplete/cap
;;;;;;  pcomplete/rake) "ruby-compilation" "rinari/util/ruby-compilation.el"
;;;;;;  (19809 16085))
;;; Generated autoloads from rinari/util/ruby-compilation.el

(autoload 'pcomplete/rake "ruby-compilation" "\
Not documented

\(fn)" nil nil)

(autoload 'pcomplete/cap "ruby-compilation" "\
Not documented

\(fn)" nil nil)

(autoload 'ruby-compilation-run "ruby-compilation" "\
Run a ruby process dumping output to a ruby compilation buffer.

\(fn CMD &optional RUBY-OPTIONS)" t nil)

(autoload 'ruby-compilation-rake "ruby-compilation" "\
Run a rake process dumping output to a ruby compilation buffer.

\(fn &optional EDIT TASK ENV-VARS)" t nil)

(autoload 'ruby-compilation-cap "ruby-compilation" "\
Run a capistrano process dumping output to a ruby compilation buffer.

\(fn &optional EDIT TASK ENV-VARS)" t nil)

(autoload 'ruby-compilation-this-buffer "ruby-compilation" "\
Run the current buffer through Ruby compilation.

\(fn)" t nil)

;;;***

;;;### (autoloads (ruby-mode) "ruby-mode" "rinari/util/ruby-mode.el"
;;;;;;  (19809 16085))
;;; Generated autoloads from rinari/util/ruby-mode.el

(autoload 'ruby-mode "ruby-mode" "\
Major mode for editing Ruby scripts.
\\[ruby-indent-line] properly indents subexpressions of multi-line
class, module, def, if, while, for, do, and case statements, taking
nesting into account.

The variable ruby-indent-level controls the amount of indentation.
\\{ruby-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))

(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(add-to-list 'interpreter-mode-alist '("rbx" . ruby-mode))

(add-to-list 'interpreter-mode-alist '("jruby" . ruby-mode))

(add-to-list 'interpreter-mode-alist '("ruby1.9" . ruby-mode))

(add-to-list 'interpreter-mode-alist '("ruby1.8" . ruby-mode))

;;;***

;;;### (autoloads (gud-speedbar-buttons) "sb-gud" "cedet/speedbar/sb-gud.el"
;;;;;;  (16232 37556))
;;; Generated autoloads from cedet/speedbar/sb-gud.el

(autoload 'gud-speedbar-buttons "sb-gud" "\
Create a speedbar display based on the current state of GUD.
If the GUD BUFFER is not running a supported debugger, then turn
off the specialized speedbar mode.

\(fn BUFFER)" nil nil)

;;;***

;;;### (autoloads (Info-speedbar-buttons Info-speedbar-browser) "sb-info"
;;;;;;  "cedet/speedbar/sb-info.el" (18873 37159))
;;; Generated autoloads from cedet/speedbar/sb-info.el

(autoload 'Info-speedbar-browser "sb-info" "\
Initialize speedbar to display an info node browser.
This will add a speedbar major display mode.

\(fn)" t nil)

(autoload 'Info-speedbar-buttons "sb-info" "\
Create a speedbar display to help navigation in an Info file.
BUFFER is the buffer speedbar is requesting buttons for.

\(fn BUFFER)" nil nil)

(eval-after-load "info" '(require 'sb-info))

;;;***

;;;### (autoloads (rmail-speedbar-buttons) "sb-rmail" "cedet/speedbar/sb-rmail.el"
;;;;;;  (15958 27182))
;;; Generated autoloads from cedet/speedbar/sb-rmail.el

(autoload 'rmail-speedbar-buttons "sb-rmail" "\
Create buttons for BUFFER containing rmail messages.
Click on the address under Reply to: to reply to this person.
Under Folders: Click a name to read it, or on the <M> to move the
current message into that RMAIL folder.

\(fn BUFFER)" nil nil)

;;;***

;;;### (autoloads (w3-speedbar-buttons) "sb-w3" "cedet/speedbar/sb-w3.el"
;;;;;;  (17213 40805))
;;; Generated autoloads from cedet/speedbar/sb-w3.el

(autoload 'w3-speedbar-buttons "sb-w3" "\
Create speedbar buttons for the current web BUFFER displayed in w3 mode.

\(fn BUFFER)" nil nil)

;;;***

;;;### (autoloads (semantic-bovinate-toplevel semantic-refresh-tags-safe
;;;;;;  semantic-fetch-tags semantic-parse-region-default) "semantic"
;;;;;;  "cedet/semantic/semantic.el" (19403 32013))
;;; Generated autoloads from cedet/semantic/semantic.el

(autoload 'semantic-parse-region-default "semantic" "\
Parse the area between START and END, and return any tags found.
If END needs to be extended due to a lexical token being too large,
it will be silently ignored.
Optional arguments:
NONTERMINAL is the rule to start parsing at if it is known.
DEPTH specifies the lexical depth to scan.
RETURNONERROR specifies that parsing should end when encountering
unterminated syntax.

\(fn START END &optional NONTERMINAL DEPTH RETURNONERROR)" nil nil)

(autoload 'semantic-fetch-tags "semantic" "\
Fetch semantic tags from the current buffer.
If the buffer cache is up to date, return that.
If the buffer cache is out of date, attempt an incremental reparse.
If the buffer has not been parsed before, or if the incremental reparse
fails, then parse the entire buffer.
If a lexical error had been previously discovered and the buffer
was marked unparseable, then do nothing, and return the cache.

\(fn)" nil nil)

(autoload 'semantic-refresh-tags-safe "semantic" "\
Refresh the current buffer's tags safely.

Return non-nil if the refresh was successful.
Return nil if there is some sort of syntax error preventing a reparse.

Does nothing if the current buffer doesn't need reparsing.

\(fn)" nil nil)

(autoload 'semantic-bovinate-toplevel "semantic" "\
Backward compatibility function.

\(fn &optional IGNORED)" nil nil)

(make-obsolete 'semantic-bovinate-toplevel 'semantic-fetch-tags)

(defsubst semantic-fetch-available-tags nil "\
Fetch available semantic tags from the current buffer.
That is, return tags currently in the cache without parsing the
current buffer.
Parse operations happen asynchronously when needed on Emacs idle time.
Use the `semantic-after-toplevel-cache-change-hook' and
`semantic-after-partial-cache-change-hook' hooks to synchronize with
new tags when they become available." semantic--buffer-cache)

;;;***

;;;### (autoloads (semanticdb-debug-file-tag-check semantic-adebug-searchdb
;;;;;;  semantic-adebug-bovinate data-debug-insert-db-and-tag-button
;;;;;;  data-debug-insert-find-results-button data-debug-insert-find-results
;;;;;;  data-debug-insert-tag-list-button data-debug-insert-tag-list
;;;;;;  data-debug-insert-tag data-debug-insert-tag-parts-from-point)
;;;;;;  "semantic-adebug" "cedet/semantic/semantic-adebug.el" (19358
;;;;;;  14566))
;;; Generated autoloads from cedet/semantic/semantic-adebug.el

(autoload 'data-debug-insert-tag-parts-from-point "semantic-adebug" "\
Call `data-debug-insert-tag-parts' based on text properties at POINT.

\(fn POINT)" nil nil)

(autoload 'data-debug-insert-tag "semantic-adebug" "\
Insert TAG into the current buffer at the current point.
PREFIX specifies text to insert in front of TAG.
PREBUTTONTEXT is text appearing btewen the prefix and TAG.
Optional PARENT is the parent tag containing TAG.
Add text properties needed to allow tag expansion later.

\(fn TAG PREFIX PREBUTTONTEXT &optional PARENT)" nil nil)

(autoload 'data-debug-insert-tag-list "semantic-adebug" "\
Insert the tag list TAGLIST with PREFIX.
Optional argument PARENT specifies the part of TAGLIST.

\(fn TAGLIST PREFIX &optional PARENT)" nil nil)

(autoload 'data-debug-insert-tag-list-button "semantic-adebug" "\
Insert a single summary of a TAGLIST.
PREFIX is the text that preceeds the button.
PREBUTTONTEXT is some text between PREFIX and the taglist button.
PARENT is the tag that represents the parent of all the tags.

\(fn TAGLIST PREFIX PREBUTTONTEXT &optional PARENT)" nil nil)

(autoload 'data-debug-insert-find-results "semantic-adebug" "\
Insert the find results FINDRES with PREFIX.

\(fn FINDRES PREFIX)" nil nil)

(autoload 'data-debug-insert-find-results-button "semantic-adebug" "\
Insert a single summary of a find results FINDRES.
PREFIX is the text that preceeds the button.
PREBUTTONTEXT is some text between prefix and the find results button.

\(fn FINDRES PREFIX PREBUTTONTEXT)" nil nil)

(autoload 'data-debug-insert-db-and-tag-button "semantic-adebug" "\
Insert a single summary of short list DBTAG of format (DB . TAG).
PREFIX is the text that preceeds the button.
PREBUTTONTEXT is some text between prefix and the find results button.

\(fn DBTAG PREFIX PREBUTTONTEXT)" nil nil)

(autoload 'semantic-adebug-bovinate "semantic-adebug" "\
The same as `bovinate'.  Display the results in a debug buffer.

\(fn)" t nil)

(autoload 'semantic-adebug-searchdb "semantic-adebug" "\
Search the semanticdb for REGEX for the current buffer.
Display the results as a debug list.

\(fn REGEX)" t nil)

(autoload 'semanticdb-debug-file-tag-check "semantic-adebug" "\
Report debug info for checking STARTFILE for up-to-date tags.

\(fn STARTFILE)" t nil)

;;;***

;;;### (autoloads (semantic-adebug-analyze semantic-analyze-current-context
;;;;;;  semantic-analyze-current-symbol) "semantic-analyze" "cedet/semantic/semantic-analyze.el"
;;;;;;  (19403 32136))
;;; Generated autoloads from cedet/semantic/semantic-analyze.el

(autoload 'semantic-analyze-current-symbol "semantic-analyze" "\
Call ANALYZEHOOKFCN after analyzing the symbol under POSITION.
The ANALYZEHOOKFCN is called with the current symbol bounds, and the
analyzed prefix.  It should take the arguments (START END PREFIX).
The ANALYZEHOOKFCN is only called if some sort of prefix with bounds was
found under POSITION.

The results of ANALYZEHOOKFCN is returned, or nil if there was nothing to
call it with.

For regular analysis, you should call `semantic-analyze-current-context'
to calculate the context information.  The purpose for this function is
to provide a large number of non-cached analysis for filtering symbols.

\(fn ANALYZEHOOKFCN &optional POSITION)" nil nil)

(autoload 'semantic-analyze-current-context "semantic-analyze" "\
Analyze the current context at optional POSITION.
If called interactively, display interesting information about POSITION
in a separate buffer.
Returns an object based on symbol `semantic-analyze-context'.

This function can be overriden with the symbol `analyze-context'.
When overriding this function, your override will be called while
cursor is at POSITION.  In addition, your function will not be called
if a cached copy of the return object is found.

\(fn &optional POSITION)" t nil)

(autoload 'semantic-adebug-analyze "semantic-analyze" "\
Perform `semantic-analyze-current-context'.
Display the results as a debug list.
Optional argument CTXT is the context to show.

\(fn &optional CTXT)" t nil)

;;;***

;;;### (autoloads (semantic-analyze-possible-completions semantic-analyze-tags-of-class-list
;;;;;;  semantic-analyze-type-constants) "semantic-analyze-complete"
;;;;;;  "cedet/semantic/semantic-analyze-complete.el" (19414 13159))
;;; Generated autoloads from cedet/semantic/semantic-analyze-complete.el

(autoload 'semantic-analyze-type-constants "semantic-analyze-complete" "\
For the tag TYPE, return any constant symbols of TYPE.
Used as options when completing.

\(fn TYPE)" nil nil)

(autoload 'semantic-analyze-tags-of-class-list "semantic-analyze-complete" "\
Return the tags in TAGS that are of classes in CLASSLIST.

\(fn TAGS CLASSLIST)" nil nil)

(autoload 'semantic-analyze-possible-completions "semantic-analyze-complete" "\
Return a list of semantic tags which are possible completions.
CONTEXT is either a position (such as point), or a precalculated
context.  Passing in a context is useful if the caller also needs
to access parts of the analysis.
The remaining FLAGS arguments are passed to the mode specific completion engine.
Bad flags should be ignored by modes that don't use them.
See `semantic-analyze-possible-completions-default' for details on the default FLAGS.

Completions run through the following filters:
  * Elements currently in scope
  * Constants currently in scope
  * Elements match the :prefix in the CONTEXT.
  * Type of the completion matches the type of the context.
Context type matching can identify the following:
  * No specific type
  * Assignment into a variable of some type.
  * Argument to a function with type constraints.
When called interactively, displays the list of possible completions
in a buffer.

\(fn CONTEXT &rest FLAGS)" t nil)

;;;***

;;;### (autoloads (semantic-analyze-debug-assist) "semantic-analyze-debug"
;;;;;;  "cedet/semantic/semantic-analyze-debug.el" (19391 52160))
;;; Generated autoloads from cedet/semantic/semantic-analyze-debug.el

(autoload 'semantic-analyze-debug-assist "semantic-analyze-debug" "\
Debug semantic analysis at the current point.

\(fn)" t nil)

;;;***

;;;### (autoloads (semantic-analyze-proto-impl-toggle semantic-analyze-current-tag
;;;;;;  semantic-analyze-tag-references) "semantic-analyze-refs"
;;;;;;  "cedet/semantic/semantic-analyze-refs.el" (19546 10573))
;;; Generated autoloads from cedet/semantic/semantic-analyze-refs.el

(autoload 'semantic-analyze-tag-references "semantic-analyze-refs" "\
Analyze the references for TAG.
Returns a class with information about TAG.

Optional argument DB is a database.  It will be used to help
locate TAG.

Use `semantic-analyze-current-tag' to debug this fcn.

\(fn TAG &optional DB)" nil nil)

(autoload 'semantic-analyze-current-tag "semantic-analyze-refs" "\
Analyze the tag under point.

\(fn)" t nil)

(autoload 'semantic-analyze-proto-impl-toggle "semantic-analyze-refs" "\
Toggle between the implementation, and a prototype of tag under point.

\(fn)" t nil)

;;;***

;;;### (autoloads (semantic-bovinate-stream semantic-lambda) "semantic-bovine"
;;;;;;  "cedet/semantic/bovine/semantic-bovine.el" (19358 14567))
;;; Generated autoloads from cedet/semantic/bovine/semantic-bovine.el

(defvar semantic-bovinate-nonterminal-check-obarray nil "\
Obarray of streams already parsed for nonterminal symbols.
Use this to detect infinite recursion during a parse.")

(autoload 'semantic-lambda "semantic-bovine" "\
Create a lambda expression to return a list including RETURN-VAL.
The return list is a lambda expression to be used in a bovine table.

\(fn &rest RETURN-VAL)" nil (quote macro))

(autoload 'semantic-bovinate-stream "semantic-bovine" "\
Bovinate STREAM, starting at the first NONTERMINAL rule.
Use `bovine-toplevel' if NONTERMINAL is not provided.
This is the core routine for converting a stream into a table.
Return the list (STREAM SEMANTIC-STREAM) where STREAM are those
elements of STREAM that have not been used.  SEMANTIC-STREAM is the
list of semantic tokens found.

\(fn STREAM &optional NONTERMINAL)" nil nil)

(defalias 'semantic-parse-stream-default 'semantic-bovinate-stream)

;;;***

;;;### (autoloads (semantic-c-add-preprocessor-symbol semantic-default-c-setup
;;;;;;  semantic-c-member-of-autocast semantic-lex-c-preprocessor-symbol-file
;;;;;;  semantic-lex-c-preprocessor-symbol-map) "semantic-c" "cedet/semantic/bovine/semantic-c.el"
;;;;;;  (19546 10661))
;;; Generated autoloads from cedet/semantic/bovine/semantic-c.el

(defvar semantic-lex-c-preprocessor-symbol-map nil "\
Table of C Preprocessor keywords used by the Semantic C lexer.
Each entry is a cons cell like this:
  ( \"KEYWORD\" . \"REPLACEMENT\" )
Where KEYWORD is the macro that gets replaced in the lexical phase,
and REPLACEMENT is a string that is inserted in its place.  Empty string
implies that the lexical analyzer will discard KEYWORD when it is encountered.

Alternately, it can be of the form:
  ( \"KEYWORD\" ( LEXSYM1 \"str\" 1 1 ) ... ( LEXSYMN \"str\" 1 1 ) )
where LEXSYM is a symbol that would normally be produced by the
lexical analyzer, such as `symbol' or `string'.  The string in the
second position is the text that makes up the replacement.  This is
the way to have multiple lexical symbols in a replacement.  Using the
first way to specify text like \"foo::bar\" would not work, because :
is a separate lexical symbol.

A quick way to see what you would need to insert is to place a
definition such as:

#define MYSYM foo::bar

into a C file, and do this:
  \\[semantic-lex-spp-describe]

The output table will describe the symbols needed.")

(custom-autoload 'semantic-lex-c-preprocessor-symbol-map "semantic-c" nil)

(defvar semantic-lex-c-preprocessor-symbol-file nil "\
List of C/C++ files that contain preprocessor macros for the C lexer.
Each entry is a filename and each file is parsed, and those macros
are included in every C/C++ file parsed by semantic.
You can use this variable instead of `semantic-lex-c-preprocessor-symbol-map'
to store your global macros in a more natural way.")

(custom-autoload 'semantic-lex-c-preprocessor-symbol-file "semantic-c" nil)

(defvar semantic-c-member-of-autocast 't "\
Non-nil means classes with a '->' operator will cast to its return type.

For Examples:

  class Foo {
    Bar *operator->();
  }

  Foo foo;

if `semantic-c-member-of-autocast' is non-nil :
  foo->[here completion will list method of Bar]

if `semantic-c-member-of-autocast' is nil :
  foo->[here completion will list method of Foo]")

(custom-autoload 'semantic-c-member-of-autocast "semantic-c" t)

(autoload 'semantic-default-c-setup "semantic-c" "\
Set up a buffer for semantic parsing of the C language.

\(fn)" nil nil)

(autoload 'semantic-c-add-preprocessor-symbol "semantic-c" "\
Add a preprocessor symbol SYM with a REPLACEMENT value.

\(fn SYM REPLACEMENT)" t nil)

(add-hook 'c-mode-hook 'semantic-default-c-setup)

(add-hook 'c++-mode-hook 'semantic-default-c-setup)

;;;***

;;;### (autoloads (semantic-chart-analyzer semantic-chart-tag-complexity
;;;;;;  semantic-chart-database-size semantic-chart-tags-by-class)
;;;;;;  "semantic-chart" "cedet/semantic/semantic-chart.el" (19358
;;;;;;  14566))
;;; Generated autoloads from cedet/semantic/semantic-chart.el

(autoload 'semantic-chart-tags-by-class "semantic-chart" "\
Create a bar chart representing the number of tags for a given tag class.
Each bar represents how many toplevel tags in TAGTABLE
exist with a given class.  See `semantic-symbol->name-assoc-list'
for tokens which will be charted.
TAGTABLE is passed to `semantic-something-to-tag-table'.

\(fn &optional TAGTABLE)" t nil)

(autoload 'semantic-chart-database-size "semantic-chart" "\
Create a bar chart representing the size of each file in semanticdb.
Each bar represents how many toplevel tags in TAGTABLE
exist in each database entry.
TAGTABLE is passed to `semantic-something-to-tag-table'.

\(fn &optional TAGTABLE)" t nil)

(autoload 'semantic-chart-tag-complexity "semantic-chart" "\
Create a bar chart representing the complexity of some tags.
Complexity is calculated for tags of CLASS.  Each bar represents
the complexity of some tag in TAGTABLE.  Only the most complex
items are charted.  TAGTABLE is passed to
`semantic-something-to-tag-table'.

\(fn &optional CLASS TAGTABLE)" t nil)

(autoload 'semantic-chart-analyzer "semantic-chart" "\
Chart the extent of the context analysis.

\(fn)" t nil)

;;;***

;;;### (autoloads (semantic-complete-self-insert semantic-complete-analyze-inline-idle
;;;;;;  semantic-complete-analyze-inline semantic-complete-analyze-and-replace
;;;;;;  semantic-complete-jump-local-members semantic-complete-jump
;;;;;;  semantic-complete-jump-local semantic-complete-inline-analyzer-idle
;;;;;;  semantic-complete-inline-analyzer semantic-complete-read-tag-analyzer
;;;;;;  semantic-complete-inline-tag-project semantic-complete-read-tag-project
;;;;;;  semantic-complete-read-tag-local-members semantic-complete-read-tag-buffer-deep
;;;;;;  semantic-complete-inline-force-display semantic-complete-read-tag-engine
;;;;;;  semantic-completion-inline-active-p) "semantic-complete"
;;;;;;  "cedet/semantic/semantic-complete.el" (19467 49856))
;;; Generated autoloads from cedet/semantic/semantic-complete.el

(autoload 'semantic-completion-inline-active-p "semantic-complete" "\
Non-nil if inline completion is active.

\(fn)" nil nil)

(autoload 'semantic-complete-read-tag-engine "semantic-complete" "\
Read a semantic tag, and return a tag for the selection.
Argument COLLECTOR is an object which can be used to calculate
a list of possible hits.  See `semantic-completion-collector-engine'
for details on COLLECTOR.
Argument DISPLAYOR is an object used to display a list of possible
completions for a given prefix.  See`semantic-completion-display-engine'
for details on DISPLAYOR.
PROMPT is a string to prompt with.
DEFAULT-TAG is a semantic tag or string to use as the default value.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
HISTORY is a symbol representing a variable to story the history in.

\(fn COLLECTOR DISPLAYOR PROMPT DEFAULT-TAG INITIAL-INPUT HISTORY)" nil nil)

(autoload 'semantic-complete-inline-force-display "semantic-complete" "\
Force the display of whatever the current completions are.
DO NOT CALL THIS IF THE INLINE COMPLETION ENGINE IS NOT ACTIVE.

\(fn)" nil nil)

(autoload 'semantic-complete-read-tag-buffer-deep "semantic-complete" "\
Ask for a tag by name from the current buffer.
Available tags are from the current buffer, at any level.
Completion options are presented in a traditional way, with highlighting
to resolve same-name collisions.
PROMPT is a string to prompt with.
DEFAULT-TAG is a semantic tag or string to use as the default value.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
HISTORY is a symbol representing a variable to store the history in.

\(fn PROMPT &optional DEFAULT-TAG INITIAL-INPUT HISTORY)" nil nil)

(autoload 'semantic-complete-read-tag-local-members "semantic-complete" "\
Ask for a tag by name from the local type members.
Available tags are from the the current scope.
Completion options are presented in a traditional way, with highlighting
to resolve same-name collisions.
PROMPT is a string to prompt with.
DEFAULT-TAG is a semantic tag or string to use as the default value.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
HISTORY is a symbol representing a variable to store the history in.

\(fn PROMPT &optional DEFAULT-TAG INITIAL-INPUT HISTORY)" nil nil)

(autoload 'semantic-complete-read-tag-project "semantic-complete" "\
Ask for a tag by name from the current project.
Available tags are from the current project, at the top level.
Completion options are presented in a traditional way, with highlighting
to resolve same-name collisions.
PROMPT is a string to prompt with.
DEFAULT-TAG is a semantic tag or string to use as the default value.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
HISTORY is a symbol representing a variable to store the history in.

\(fn PROMPT &optional DEFAULT-TAG INITIAL-INPUT HISTORY)" nil nil)

(autoload 'semantic-complete-inline-tag-project "semantic-complete" "\
Complete a symbol name by name from within the current project.
This is similar to `semantic-complete-read-tag-project', except
that the completion interaction is in the buffer where the context
was calculated from.
Customize `semantic-complete-inline-analyzer-displayor-class'
to control how completion options are displayed.
See `semantic-complete-inline-tag-engine' for details on how
completion works.

\(fn)" nil nil)

(autoload 'semantic-complete-read-tag-analyzer "semantic-complete" "\
Ask for a tag by name based on the current context.
The function `semantic-analyze-current-context' is used to
calculate the context.  `semantic-analyze-possible-completions' is used
to generate the list of possible completions.
PROMPT is the first part of the prompt.  Additional prompt
is added based on the contexts full prefix.
CONTEXT is the semantic analyzer context to start with.
HISTORY is a symbol representing a variable to store the history in.
usually a default-tag and initial-input are available for completion
prompts.  these are calculated from the CONTEXT variable passed in.

\(fn PROMPT &optional CONTEXT HISTORY)" nil nil)

(autoload 'semantic-complete-inline-analyzer "semantic-complete" "\
Complete a symbol name by name based on the current context.
This is similar to `semantic-complete-read-tag-analyze', except
that the completion interaction is in the buffer where the context
was calculated from.
CONTEXT is the semantic analyzer context to start with.
Customize `semantic-complete-inline-analyzer-displayor-class'
to control how completion options are displayed.

See `semantic-complete-inline-tag-engine' for details on how
completion works.

\(fn CONTEXT)" nil nil)

(autoload 'semantic-complete-inline-analyzer-idle "semantic-complete" "\
Complete a symbol name by name based on the current context for idle time.
CONTEXT is the semantic analyzer context to start with.
This function is used from `semantic-idle-completions-mode'.

This is the same as `semantic-complete-inline-analyzer', except that
it uses `semantic-complete-inline-analyzer-idle-displayor-class'
to control how completions are displayed.

See `semantic-complete-inline-tag-engine' for details on how
completion works.

\(fn CONTEXT)" nil nil)

(autoload 'semantic-complete-jump-local "semantic-complete" "\
Jump to a semantic symbol.

\(fn)" t nil)

(autoload 'semantic-complete-jump "semantic-complete" "\
Jump to a semantic symbol.

\(fn)" t nil)

(autoload 'semantic-complete-jump-local-members "semantic-complete" "\
Jump to a semantic symbol.

\(fn)" t nil)

(autoload 'semantic-complete-analyze-and-replace "semantic-complete" "\
Perform prompt completion to do in buffer completion.
`semantic-analyze-possible-completions' is used to determine the
possible values.
The minibuffer is used to perform the completion.
The result is inserted as a replacement of the text that was there.

\(fn)" t nil)

(autoload 'semantic-complete-analyze-inline "semantic-complete" "\
Perform prompt completion to do in buffer completion.
`semantic-analyze-possible-completions' is used to determine the
possible values.
The function returns immediately, leaving the buffer in a mode that
will perform the completion.
Configure `semantic-complete-inline-analyzer-displayor-class' to change
how completion options are displayed.

\(fn)" t nil)

(autoload 'semantic-complete-analyze-inline-idle "semantic-complete" "\
Perform prompt completion to do in buffer completion.
`semantic-analyze-possible-completions' is used to determine the
possible values.
The function returns immediately, leaving the buffer in a mode that
will perform the completion.
Configure `semantic-complete-inline-analyzer-idle-displayor-class'
to change how completion options are displayed.

\(fn)" t nil)

(autoload 'semantic-complete-self-insert "semantic-complete" "\
Like `self-insert-command', but does completion afterwards.
ARG is passed to `self-insert-command'.  If ARG is nil,
use `semantic-complete-analyze-inline' to complete.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil "semantic-ctxt" "cedet/semantic/semantic-ctxt.el"
;;;;;;  (19358 14566))
;;; Generated autoloads from cedet/semantic/semantic-ctxt.el

(defvar semantic-command-separation-character ";" "\
String which indicates the end of a command.
Used for identifying the end of a single command.")

;;;***

;;;### (autoloads (semantic-tag-customize semantic-tag-widget-to-external
;;;;;;  semantic-tag-widget-to-internal semantic-tag-widget-value-get
;;;;;;  semantic-tag-widget-value-create semantic-tag-widget-match)
;;;;;;  "semantic-custom" "cedet/semantic/semantic-custom.el" (18878
;;;;;;  61311))
;;; Generated autoloads from cedet/semantic/semantic-custom.el

(define-widget 'tag-edit 'group "\
Abstractly modify a Semantic Tag." :tag "Tag" :format "%v" :convert-widget (quote widget-types-convert-widget) :value-create (quote semantic-tag-widget-value-create) :value-get (quote semantic-tag-widget-value-get) :value-delete (quote widget-children-value-delete) :validate (quote widget-children-validate) :match (quote semantic-tag-widget-match) :clone-object-children nil)

(autoload 'semantic-tag-widget-match "semantic-custom" "\
Match infor for WIDGET against VALUE.

\(fn WIDGET VALUE)" nil nil)

(autoload 'semantic-tag-widget-value-create "semantic-custom" "\
Create the value of WIDGET.

\(fn WIDGET)" nil nil)

(autoload 'semantic-tag-widget-value-get "semantic-custom" "\
Get the value out of WIDGET.

\(fn WIDGET)" nil nil)

(define-widget 'tag 'tag-edit "\
A Semantic Tag." :format "%{%t%}:
%v" :value-to-internal (quote semantic-tag-widget-to-internal) :value-to-external (quote semantic-tag-widget-to-external) :close-object-children t)

(autoload 'semantic-tag-widget-to-internal "semantic-custom" "\
For WIDGET, convert VALUE to a safe representation.

\(fn WIDGET VALUE)" nil nil)

(autoload 'semantic-tag-widget-to-external "semantic-custom" "\
For WIDGET, convert VALUE from the abstract value.

\(fn WIDGET VALUE)" nil nil)

(autoload 'semantic-tag-customize "semantic-custom" "\
Customize TAG.
When the user clicks 'ACCEPT', then the location where TAG
is stored is directly modified.
If TAG is not provided, then the tag under point is used.

\(fn &optional TAG)" t nil)

;;;***

;;;### (autoloads (semantic-debug semantic-debug-break) "semantic-debug"
;;;;;;  "cedet/semantic/semantic-debug.el" (19358 14566))
;;; Generated autoloads from cedet/semantic/semantic-debug.el

(defvar semantic-debug-parser-source nil "\
For any buffer, the file name (no path) of the parser.
This would be a parser for a specific language, not the source
to one of the parser generators.")

(make-variable-buffer-local 'semantic-debug-parser-source)

(defvar semantic-debug-parser-class nil "\
Class to create when building a debug parser object.")

(make-variable-buffer-local 'semantic-debug-parser-class)

(defvar semantic-debug-enabled nil "\
Non-nil when debugging a parser.")

(autoload 'semantic-debug-break "semantic-debug" "\
Stop parsing now at FRAME.
FRAME is an object that represents the parser's view of the
current state of the world.
This function enters a recursive edit.  It returns
on an `exit-recursive-edit', or if someone uses one
of the `semantic-debug-mode' commands.
It returns the command specified.  Parsers need to take action
on different types of return values.

\(fn FRAME)" nil nil)

(autoload 'semantic-debug "semantic-debug" "\
Parse the current buffer and run in debug mode.

\(fn)" t nil)

;;;***

;;;### (autoloads (semantic-tag-folded-p semantic-set-tag-folded
;;;;;;  semantic-tag-delete-secondary-overlay semantic-tag-get-secondary-overlay
;;;;;;  semantic-tag-create-secondary-overlay semantic-tag-secondary-overlays
;;;;;;  semantic-tag-read-only-p semantic-set-tag-read-only semantic-tag-intangible-p
;;;;;;  semantic-set-tag-intangible semantic-tag-invisible-p semantic-set-tag-invisible
;;;;;;  semantic-set-tag-face semantic-momentary-highlight-tag semantic-momentary-highlight-one-tag-line
;;;;;;  semantic-unhighlight-tag semantic-highlight-tag) "semantic-decorate"
;;;;;;  "cedet/semantic/semantic-decorate.el" (19269 17789))
;;; Generated autoloads from cedet/semantic/semantic-decorate.el

(autoload 'semantic-highlight-tag "semantic-decorate" "\
Specify that TAG should be highlighted.
Optional FACE specifies the face to use.

\(fn TAG &optional FACE)" nil nil)

(autoload 'semantic-unhighlight-tag "semantic-decorate" "\
Unhighlight TAG, restoring its previous face.

\(fn TAG)" nil nil)

(autoload 'semantic-momentary-highlight-one-tag-line "semantic-decorate" "\
Highlight the first line of TAG, unhighlighting before next command.
Optional argument FACE specifies the face to do the highlighting.

\(fn TAG &optional FACE)" nil nil)

(autoload 'semantic-momentary-highlight-tag "semantic-decorate" "\
Highlight TAG, removing highlighting when the user hits a key.
Optional argument FACE is the face to use for highlighting.
If FACE is not specified, then `highlight' will be used.

\(fn TAG &optional FACE)" nil nil)

(autoload 'semantic-set-tag-face "semantic-decorate" "\
Specify that TAG should use FACE for display.

\(fn TAG FACE)" nil nil)

(autoload 'semantic-set-tag-invisible "semantic-decorate" "\
Enable the text in TAG to be made invisible.
If VISIBLE is non-nil, make the text visible.

\(fn TAG &optional VISIBLE)" nil nil)

(autoload 'semantic-tag-invisible-p "semantic-decorate" "\
Return non-nil if TAG is invisible.

\(fn TAG)" nil nil)

(autoload 'semantic-set-tag-intangible "semantic-decorate" "\
Enable the text in TAG to be made intangible.
If TANGIBLE is non-nil, make the text visible.
This function does not have meaning in XEmacs because it seems that
the extent 'intangible' property does not exist.

\(fn TAG &optional TANGIBLE)" nil nil)

(autoload 'semantic-tag-intangible-p "semantic-decorate" "\
Return non-nil if TAG is intangible.
This function does not have meaning in XEmacs because it seems that
the extent 'intangible' property does not exist.

\(fn TAG)" nil nil)

(autoload 'semantic-set-tag-read-only "semantic-decorate" "\
Enable the text in TAG to be made read-only.
Optional argument WRITABLE should be non-nil to make the text writable
instead of read-only.

\(fn TAG &optional WRITABLE)" nil nil)

(autoload 'semantic-tag-read-only-p "semantic-decorate" "\
Return non-nil if the current TAG is marked read only.

\(fn TAG)" nil nil)

(semantic-alias-obsolete 'semantic-highlight-token 'semantic-highlight-tag)

(semantic-alias-obsolete 'semantic-unhighlight-token 'semantic-unhighlight-tag)

(semantic-alias-obsolete 'semantic-momentary-highlight-token 'semantic-momentary-highlight-tag)

(semantic-alias-obsolete 'semantic-set-token-face 'semantic-set-tag-face)

(semantic-alias-obsolete 'semantic-set-token-invisible 'semantic-set-tag-invisible)

(semantic-alias-obsolete 'semantic-token-invisible-p 'semantic-tag-invisible-p)

(semantic-alias-obsolete 'semantic-set-token-intangible 'semantic-set-tag-intangible)

(semantic-alias-obsolete 'semantic-token-intangible-p 'semantic-tag-intangible-p)

(semantic-alias-obsolete 'semantic-set-token-read-only 'semantic-set-tag-read-only)

(semantic-alias-obsolete 'semantic-token-read-only-p 'semantic-tag-read-only-p)

(autoload 'semantic-tag-secondary-overlays "semantic-decorate" "\
Return a list of secondary overlays active on TAG.

\(fn TAG)" nil nil)

(autoload 'semantic-tag-create-secondary-overlay "semantic-decorate" "\
Create a secondary overlay for TAG.
Returns an overlay.  The overlay is also saved in TAG.
LINK-HOOK is a function called whenever TAG is to be linked into
a buffer.  It should take TAG and OVERLAY as arguments.
The LINK-HOOK should be used to position and set properties on the
generated secondary overlay.

\(fn TAG &optional LINK-HOOK)" nil nil)

(autoload 'semantic-tag-get-secondary-overlay "semantic-decorate" "\
Return secondary overlays from TAG with PROPERTY.
PROPERTY is a symbol and all overlays with that symbol are returned..

\(fn TAG PROPERTY)" nil nil)

(autoload 'semantic-tag-delete-secondary-overlay "semantic-decorate" "\
Delete from TAG the secondary overlay OVERLAY-OR-PROPERTY.
If OVERLAY-OR-PROPERTY is an overlay, delete that overlay.
If OVERLAY-OR-PROPERTY is a symbol, find the overlay with that property.

\(fn TAG OVERLAY-OR-PROPERTY)" nil nil)

(autoload 'semantic-set-tag-folded "semantic-decorate" "\
Fold TAG, such that only the first line of text is shown.
Optional argument FOLDED should be non-nil to fold the tag.
nil implies the tag should be fully shown.

\(fn TAG &optional FOLDED)" nil nil)

(autoload 'semantic-tag-folded-p "semantic-decorate" "\
Non-nil if TAG is currently folded.

\(fn TAG)" nil nil)

;;;***

;;;### (autoloads (semantic-decoration-unparsed-include-do-reset
;;;;;;  semantic-decoration-include-visit) "semantic-decorate-include"
;;;;;;  "cedet/semantic/semantic-decorate-include.el" (19390 35142))
;;; Generated autoloads from cedet/semantic/semantic-decorate-include.el

(autoload 'semantic-decoration-include-visit "semantic-decorate-include" "\
Visit the included file at point.

\(fn)" t nil)

(autoload 'semantic-decoration-unparsed-include-do-reset "semantic-decorate-include" "\
Do a reset of unparsed includes in the current buffer.

\(fn)" nil nil)

;;;***

;;;### (autoloads (semantic-build-decoration-mode-menu semantic-decoration-mode
;;;;;;  global-semantic-decoration-mode global-semantic-decoration-mode
;;;;;;  semantic-decorate-flush-pending-decorations) "semantic-decorate-mode"
;;;;;;  "cedet/semantic/semantic-decorate-mode.el" (19390 34976))
;;; Generated autoloads from cedet/semantic/semantic-decorate-mode.el

(autoload 'semantic-decorate-flush-pending-decorations "semantic-decorate-mode" "\
Flush any pending decorations for BUFFER.
Flush functions from `semantic-decorate-pending-decoration-hook'.

\(fn &optional BUFFER)" nil nil)

(defvar global-semantic-decoration-mode nil "\
*If non-nil, enable global use of command `semantic-decoration-mode'.
When this mode is activated, decorations specified by
`semantic-decoration-styles'.")

(custom-autoload 'global-semantic-decoration-mode "semantic-decorate-mode" nil)

(autoload 'global-semantic-decoration-mode "semantic-decorate-mode" "\
Toggle global use of option `semantic-decoration-mode'.
Decoration mode turns on all active decorations as specified
by `semantic-decoration-styles'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

(defvar semantic-decoration-mode nil "\
Non-nil if command `semantic-decoration-mode' is enabled.
Use the command `semantic-decoration-mode' to change this variable.")

(autoload 'semantic-decoration-mode "semantic-decorate-mode" "\
Minor mode for decorating tags.
Decorations are specified in `semantic-decoration-styles'.
You can define new decoration styles with
`define-semantic-decoration-style'.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\(fn &optional ARG)" t nil)

(autoload 'semantic-build-decoration-mode-menu "semantic-decorate-mode" "\
Create a menu listing all the known decorations for toggling.
IGNORE any input arguments.

\(fn &rest IGNORE)" nil nil)

;;;***

;;;### (autoloads (semantic-dependency-find-file-on-path semantic-customize-system-include-path
;;;;;;  semantic-reset-system-include semantic-remove-system-include
;;;;;;  semantic-add-system-include defcustom-mode-local-semantic-dependency-system-include-path)
;;;;;;  "semantic-dep" "cedet/semantic/semantic-dep.el" (19358 14566))
;;; Generated autoloads from cedet/semantic/semantic-dep.el

(defvar semantic-dependency-include-path nil "\
Defines the include path used when searching for files.
This should be a list of directories to search which is specific
to the file being included.

If `semantic-dependency-tag-file' is overridden for a given
language, this path is most likely ignored.

The above function, regardless of being overridden, caches the
located dependency file location in the tag property
`dependency-file'.  If you override this function, you do not
need to implement your own cache.  Each time the buffer is fully
reparsed, the cache will be reset.

TODO: use ffap.el to locate such items?

NOTE: Obsolete this, or use as special user")

(defvar semantic-dependency-system-include-path nil "\
Defines the system include path.
This should be set with either `defvar-mode-local', or with
`semantic-add-system-include'.

For mode authors, use
`defcustom-mode-local-semantic-dependency-system-include-path'
to create a mode-specific variable to control this.

When searching for a file associated with a name found in an tag of
class include, this path will be inspected for includes of type
`system'.  Some include tags are agnostic to this setting and will
check both the project and system directories.")

(autoload 'defcustom-mode-local-semantic-dependency-system-include-path "semantic-dep" "\
Create a mode-local value of the system-dependency include path.
MODE is the `major-mode' this name/value pairs is for.
NAME is the name of the customizable value users will use.
VALUE is the path (a list of strings) to add.
DOCSTRING is a documentation string applied to the variable NAME
users will customize.

Creates a customizable variable users can customize that will
keep semantic data structures up to date.

\(fn MODE NAME VALUE &optional DOCSTRING)" nil (quote macro))

(autoload 'semantic-add-system-include "semantic-dep" "\
Add a system include DIR to path for MODE.
Modifies a mode-local version of `semantic-dependency-system-include-path'.

Changes made by this function are not persistent.

\(fn DIR &optional MODE)" t nil)

(autoload 'semantic-remove-system-include "semantic-dep" "\
Add a system include DIR to path for MODE.
Modifies a mode-local version of`semantic-dependency-system-include-path'.

Changes made by this function are not persistent.

\(fn DIR &optional MODE)" t nil)

(autoload 'semantic-reset-system-include "semantic-dep" "\
Reset the system include list to empty for MODE.
Modifies a mode-local version of
`semantic-dependency-system-include-path'.

\(fn &optional MODE)" t nil)

(autoload 'semantic-customize-system-include-path "semantic-dep" "\
Customize the include path for this `major-mode'.
To create a customizable include path for a major MODE, use the
macro `defcustom-mode-local-semantic-dependency-system-include-path'.

\(fn &optional MODE)" t nil)

(autoload 'semantic-dependency-find-file-on-path "semantic-dep" "\
Return an expanded file name for FILE on available paths.
If SYSTEMP is true, then only search system paths.
If optional argument MODE is non-nil, then derive paths from the
provided mode, not from the current major mode.

\(fn FILE SYSTEMP &optional MODE)" nil nil)

;;;***

;;;### (autoloads (semantic-documentation-comment-preceeding-tag
;;;;;;  semantic-documentation-for-tag) "semantic-doc" "cedet/semantic/semantic-doc.el"
;;;;;;  (19421 29986))
;;; Generated autoloads from cedet/semantic/semantic-doc.el

(autoload 'semantic-documentation-for-tag "semantic-doc" "\
Find documentation from TAG and return it as a clean string.
TAG might have DOCUMENTATION set in it already.  If not, there may be
some documentation in a comment preceding TAG's definition which we
can look for.  When appropriate, this can be overridden by a language specific
enhancement.
Optional argument NOSNARF means to only return the lexical analyzer token for it.
If nosnarf if 'lex, then only return the lex token.

\(fn &optional TAG NOSNARF)" nil nil)

(autoload 'semantic-documentation-comment-preceeding-tag "semantic-doc" "\
Find a comment preceding TAG.
If TAG is nil.  use the tag under point.
Searches the space between TAG and the preceding tag for a comment,
and converts the comment into clean documentation.
Optional argument NOSNARF with a value of 'lex means to return
just the lexical token and not the string.

\(fn &optional TAG NOSNARF)" nil nil)

(semantic-alias-obsolete 'semantic-find-documentation 'semantic-documentation-for-tag)

;;;***

;;;### (autoloads (semantic-load-enable-primary-exuberent-ctags-support)
;;;;;;  "semantic-ectag-lang" "cedet/semantic/ctags/semantic-ectag-lang.el"
;;;;;;  (19390 36925))
;;; Generated autoloads from cedet/semantic/ctags/semantic-ectag-lang.el

(autoload 'semantic-load-enable-primary-exuberent-ctags-support "semantic-ectag-lang" "\
Enable all ectag supported parsers for new languages.
This is support for any language that does not have a regular
semantic parser.

\(fn)" t nil)

;;;***

;;;### (autoloads (semantic-load-enable-secondary-exuberent-ctags-support)
;;;;;;  "semantic-ectag-lang2" "cedet/semantic/ctags/semantic-ectag-lang2.el"
;;;;;;  (18869 46253))
;;; Generated autoloads from cedet/semantic/ctags/semantic-ectag-lang2.el

(autoload 'semantic-load-enable-secondary-exuberent-ctags-support "semantic-ectag-lang2" "\
Enable exuberent ctags support as a secondary parser.
This is for semanticdb out-of-buffer parsing support.
Any mode that has been tested to work will be added to this function.

\(fn)" t nil)

;;;***

;;;### (autoloads (semantic-ectag-parse-buffer) "semantic-ectag-parse"
;;;;;;  "cedet/semantic/ctags/semantic-ectag-parse.el" (19390 35527))
;;; Generated autoloads from cedet/semantic/ctags/semantic-ectag-parse.el

(autoload 'semantic-ectag-parse-buffer "semantic-ectag-parse" "\
Execute Exuberent CTags on this buffer.
Convert the output tags into Semantic tags.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "semantic-ede-grammar" "cedet/semantic/semantic-ede-grammar.el"
;;;;;;  (19358 14566))
;;; Generated autoloads from cedet/semantic/semantic-ede-grammar.el

(eieio-defclass-autoload 'semantic-ede-proj-target-grammar '(ede-proj-target-makefile) "semantic-ede-grammar" "This target consists of a group of grammar files.\nA grammar target consists of grammar files that build Emacs Lisp programs for\nparsing different languages.")

(autoload 'ede-proj-target-elisp "semantic-ede-proj-target-grammar" "\
Target class for Emacs/Semantic grammar files." nil nil)

(eval-after-load "ede-proj" '(require 'semantic-ede-grammar))

;;;***

;;;### (autoloads (semantic-edits-incremental-parser semantic-edits-flush-changes
;;;;;;  semantic-edits-change-function-handle-changes semantic-change-function
;;;;;;  semantic-edits-verbose-flag) "semantic-edit" "cedet/semantic/semantic-edit.el"
;;;;;;  (19373 12954))
;;; Generated autoloads from cedet/semantic/semantic-edit.el

(defvar semantic-edits-verbose-flag nil "\
Non-nil means the incremental parser is verbose.
If nil, errors are still displayed, but informative messages are not.")

(custom-autoload 'semantic-edits-verbose-flag "semantic-edit" t)

(autoload 'semantic-change-function "semantic-edit" "\
Provide a mechanism for semantic tag management.
Argument START, END, and LENGTH specify the bounds of the change.

\(fn START END LENGTH)" nil nil)

(autoload 'semantic-edits-change-function-handle-changes "semantic-edit" "\
Run whenever a buffer controlled by `semantic-mode' change.
Tracks when and how the buffer is re-parsed.
Argument START, END, and LENGTH specify the bounds of the change.

\(fn START END LENGTH)" nil nil)

(autoload 'semantic-edits-flush-changes "semantic-edit" "\
Flush the changes in the current buffer.

\(fn)" nil nil)

(autoload 'semantic-edits-incremental-parser "semantic-edit" "\
Incrementally reparse the current buffer.
Incremental parser allows semantic to only reparse those sections of
the buffer that have changed.  This function depends on
`semantic-edits-change-function-handle-changes' setting up change
overlays in the current buffer.  Those overlays are analyzed against
the semantic cache to see what needs to be changed.

\(fn)" nil nil)

(defalias 'semantic-parse-changes-default 'semantic-edits-incremental-parser)

(add-hook 'semantic-change-hooks #'semantic-edits-change-function-handle-changes)

(add-hook 'semantic-before-toplevel-cache-flush-hook #'semantic-edits-flush-changes)

;;;***

;;;### (autoloads (semantic-default-elisp-setup) "semantic-el" "cedet/semantic/bovine/semantic-el.el"
;;;;;;  (19373 12958))
;;; Generated autoloads from cedet/semantic/bovine/semantic-el.el

(autoload 'semantic-default-elisp-setup "semantic-el" "\
Setup hook function for Emacs Lisp files and Semantic.

\(fn)" nil nil)

(add-hook 'emacs-lisp-mode-hook 'semantic-default-elisp-setup)

(add-hook 'lisp-mode-hook 'semantic-default-elisp-setup)

(eval-after-load "semanticdb" '(require 'semanticdb-el))

;;;***

;;;### (autoloads (semantic-elp-load-old-run semantic-elp-analyze)
;;;;;;  "semantic-elp" "cedet/semantic/semantic-elp.el" (19114 57201))
;;; Generated autoloads from cedet/semantic/semantic-elp.el

(autoload 'semantic-elp-analyze "semantic-elp" "\
Run the analyzer, using ELP to measure performance.

\(fn)" t nil)

(autoload 'semantic-elp-load-old-run "semantic-elp" "\
Load an old run from FILE, and show it.

\(fn FILE)" t nil)

;;;***

;;;### (autoloads (semantic-brute-find-innermost-tag-by-position
;;;;;;  semantic-brute-find-tag-by-position semantic-brute-find-first-tag-by-function
;;;;;;  semantic-brute-find-tag-by-function semantic-brute-find-tag-by-attribute-value
;;;;;;  semantic-brute-find-tag-by-attribute semantic-brute-find-tag-by-property
;;;;;;  semantic-brute-find-tag-by-name-regexp semantic-brute-find-tag-by-type-regexp
;;;;;;  semantic-brute-find-tag-by-type semantic-brute-find-tag-standard
;;;;;;  semantic-brute-find-tag-by-class semantic-brute-find-first-tag-by-name
;;;;;;  semantic-deep-find-tags-by-name-regexp semantic-deep-find-tags-for-completion
;;;;;;  semantic-deep-find-tags-by-name semantic-find-tags-by-scope-protection
;;;;;;  semantic-find-tags-of-compound-type semantic-find-tags-by-type
;;;;;;  semantic-find-tags-by-class semantic-find-tags-by-name-regexp
;;;;;;  semantic-find-tags-for-completion semantic-find-tags-by-name
;;;;;;  semantic-current-tag-of-class semantic-current-tag-parent
;;;;;;  semantic-current-tag semantic-find-tag-parent-by-overlay
;;;;;;  semantic-find-tag-by-overlay-prev semantic-find-tag-by-overlay-next
;;;;;;  semantic-find-tag-by-overlay-in-region semantic-find-tag-by-overlay)
;;;;;;  "semantic-find" "cedet/semantic/semantic-find.el" (19358
;;;;;;  14566))
;;; Generated autoloads from cedet/semantic/semantic-find.el

(autoload 'semantic-find-tag-by-overlay "semantic-find" "\
Find all tags covering POSITIONORMARKER by using overlays.
If POSITIONORMARKER is nil, use the current point.
Optional BUFFER is used if POSITIONORMARKER is a number, otherwise the current
buffer is used.  This finds all tags covering the specified position
by checking for all overlays covering the current spot.  They are then sorted
from largest to smallest via the start location.

\(fn &optional POSITIONORMARKER BUFFER)" nil nil)

(autoload 'semantic-find-tag-by-overlay-in-region "semantic-find" "\
Find all tags which exist in whole or in part between START and END.
Uses overlays to determine position.
Optional BUFFER argument specifies the buffer to use.

\(fn START END &optional BUFFER)" nil nil)

(autoload 'semantic-find-tag-by-overlay-next "semantic-find" "\
Find the next tag after START in BUFFER.
If START is in an overlay, find the tag which starts next,
not the current tag.

\(fn &optional START BUFFER)" nil nil)

(autoload 'semantic-find-tag-by-overlay-prev "semantic-find" "\
Find the next tag before START in BUFFER.
If START is in an overlay, find the tag which starts next,
not the current tag.

\(fn &optional START BUFFER)" nil nil)

(autoload 'semantic-find-tag-parent-by-overlay "semantic-find" "\
Find the parent of TAG by overlays.
Overlays are a fast way of finding this information for active buffers.

\(fn TAG)" nil nil)

(autoload 'semantic-current-tag "semantic-find" "\
Return the current tag in the current buffer.
If there are more than one in the same location, return the
smallest tag.  Return nil if there is no tag here.

\(fn)" nil nil)

(autoload 'semantic-current-tag-parent "semantic-find" "\
Return the current tags parent in the current buffer.
A tag's parent would be a containing structure, such as a type
containing a field.  Return nil if there is no parent.

\(fn)" nil nil)

(autoload 'semantic-current-tag-of-class "semantic-find" "\
Return the current (smallest) tags of CLASS in the current buffer.
If the smallest tag is not of type CLASS, keep going upwards until one
is found.
Uses `semantic-tag-class' for classification.

\(fn CLASS)" nil nil)

(defsubst semantic-find-first-tag-by-name (name &optional table) "\
Find the first tag with NAME in TABLE.
NAME is a string.
TABLE is a semantic tags table.  See `semantic-something-to-tag-table'.
This routine uses `assoc' to quickly find the first matching entry." (funcall (if semantic-case-fold (quote assoc-ignore-case) (quote assoc)) name (semantic-something-to-tag-table table)))

(autoload 'semantic-find-tags-by-name "semantic-find" "\
Find all tags with NAME in TABLE.
NAME is a string.
TABLE is a tag table.  See `semantic-something-to-tag-table'.

\(fn NAME &optional TABLE)" nil (quote macro))

(autoload 'semantic-find-tags-for-completion "semantic-find" "\
Find all tags whose name begins with PREFIX in TABLE.
PREFIX is a string.
TABLE is a tag table.  See `semantic-something-to-tag-table'.
While it would be nice to use `try-completion' or `all-completions',
those functions do not return the tags, only a string.
Uses `compare-strings' for fast comparison.

\(fn PREFIX &optional TABLE)" nil (quote macro))

(autoload 'semantic-find-tags-by-name-regexp "semantic-find" "\
Find all tags with name matching REGEXP in TABLE.
REGEXP is a string containing a regular expression,
TABLE is a tag table.  See `semantic-something-to-tag-table'.
Consider using `semantic-find-tags-for-completion' if you are
attempting to do completions.

\(fn REGEXP &optional TABLE)" nil (quote macro))

(autoload 'semantic-find-tags-by-class "semantic-find" "\
Find all tags of class CLASS in TABLE.
CLASS is a symbol representing the class of the token, such as
'variable, of 'function..
TABLE is a tag table.  See `semantic-something-to-tag-table'.

\(fn CLASS &optional TABLE)" nil (quote macro))

(autoload 'semantic-find-tags-by-type "semantic-find" "\
Find all tags of with a type TYPE in TABLE.
TYPE is a string or tag representing a data type as defined in the
language the tags were parsed from, such as \"int\", or perhaps
a tag whose name is that of a struct or class.
TABLE is a tag table.  See `semantic-something-to-tag-table'.

\(fn TYPE &optional TABLE)" nil (quote macro))

(autoload 'semantic-find-tags-of-compound-type "semantic-find" "\
Find all tags which are a compound type in TABLE.
Compound types are structures, or other data type which
is not of a primitive nature, such as int or double.
Used in completion.

\(fn &optional TABLE)" nil (quote macro))

(autoload 'semantic-find-tags-by-scope-protection "semantic-find" "\
Find all tags accessible by SCOPEPROTECTION.
SCOPEPROTECTION is a symbol which can be returned by the method
`semantic-tag-protection'.  A hard-coded order is used to determine a match.
PARENT is a tag representing the PARENT slot needed for
`semantic-tag-protection'.
TABLE is a list of tags (a subset of PARENT members) to scan.  If TABLE is nil,
the type members of PARENT are used.
See `semantic-tag-protected-p' for details on which tags are returned.

\(fn SCOPEPROTECTION PARENT &optional TABLE)" nil nil)

(defsubst semantic-find-tags-included (&optional table) "\
Find all tags in TABLE that are of the 'include class.
TABLE is a tag table.  See `semantic-something-to-tag-table'." (semantic-find-tags-by-class (quote include) table))

(autoload 'semantic-deep-find-tags-by-name "semantic-find" "\
Find all tags with NAME in TABLE.
Search in top level tags, and their components, in TABLE.
NAME is a string.
TABLE is a tag table.  See `semantic-flatten-tags-table'.
See also `semantic-find-tags-by-name'.

\(fn NAME &optional TABLE)" nil (quote macro))

(autoload 'semantic-deep-find-tags-for-completion "semantic-find" "\
Find all tags whose name begins with PREFIX in TABLE.
Search in top level tags, and their components, in TABLE.
TABLE is a tag table.  See `semantic-flatten-tags-table'.
See also `semantic-find-tags-for-completion'.

\(fn PREFIX &optional TABLE)" nil (quote macro))

(autoload 'semantic-deep-find-tags-by-name-regexp "semantic-find" "\
Find all tags with name matching REGEXP in TABLE.
Search in top level tags, and their components, in TABLE.
REGEXP is a string containing a regular expression,
TABLE is a tag table.  See `semantic-flatten-tags-table'.
See also `semantic-find-tags-by-name-regexp'.
Consider using `semantic-deep-find-tags-for-completion' if you are
attempting to do completions.

\(fn REGEXP &optional TABLE)" nil (quote macro))

(autoload 'semantic-brute-find-first-tag-by-name "semantic-find" "\
Find a tag NAME within STREAMORBUFFER.  NAME is a string.
If SEARCH-PARTS is non-nil, search children of tags.
If SEARCH-INCLUDE was never implemented.

Use `semantic-find-first-tag-by-name' instead.

\(fn NAME STREAMORBUFFER &optional SEARCH-PARTS SEARCH-INCLUDE)" nil nil)

(autoload 'semantic-brute-find-tag-by-class "semantic-find" "\
Find all tags with a class CLASS within STREAMORBUFFER.
CLASS is a symbol representing the class of the tags to find.
See `semantic-tag-class'.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'.

Use `semantic-find-tag-by-class' instead.

\(fn CLASS STREAMORBUFFER &optional SEARCH-PARTS SEARCH-INCLUDES)" nil (quote macro))

(autoload 'semantic-brute-find-tag-standard "semantic-find" "\
Find all tags in STREAMORBUFFER which define simple class types.
See `semantic-tag-class'.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'.

\(fn STREAMORBUFFER &optional SEARCH-PARTS SEARCH-INCLUDES)" nil (quote macro))

(autoload 'semantic-brute-find-tag-by-type "semantic-find" "\
Find all tags with type TYPE within STREAMORBUFFER.
TYPE is a string which is the name of the type of the tags returned.
See `semantic-tag-type'.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'.

\(fn TYPE STREAMORBUFFER &optional SEARCH-PARTS SEARCH-INCLUDES)" nil nil)

(autoload 'semantic-brute-find-tag-by-type-regexp "semantic-find" "\
Find all tags with type matching REGEXP within STREAMORBUFFER.
REGEXP is a regular expression  which matches the  name of the type of the
tags returned.  See `semantic-tag-type'.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'.

\(fn REGEXP STREAMORBUFFER &optional SEARCH-PARTS SEARCH-INCLUDES)" nil nil)

(autoload 'semantic-brute-find-tag-by-name-regexp "semantic-find" "\
Find all tags whose name match REGEX in STREAMORBUFFER.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'.

\(fn REGEX STREAMORBUFFER &optional SEARCH-PARTS SEARCH-INCLUDES)" nil nil)

(autoload 'semantic-brute-find-tag-by-property "semantic-find" "\
Find all tags with PROPERTY equal to VALUE in STREAMORBUFFER.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'.

\(fn PROPERTY VALUE STREAMORBUFFER &optional SEARCH-PARTS SEARCH-INCLUDES)" nil nil)

(autoload 'semantic-brute-find-tag-by-attribute "semantic-find" "\
Find all tags with a given ATTR in STREAMORBUFFER.
ATTR is a symbol key into the attributes list.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'.

\(fn ATTR STREAMORBUFFER &optional SEARCH-PARTS SEARCH-INCLUDES)" nil nil)

(autoload 'semantic-brute-find-tag-by-attribute-value "semantic-find" "\
Find all tags with a given ATTR equal to VALUE in STREAMORBUFFER.
ATTR is a symbol key into the attributes list.
VALUE is the value that ATTR should match.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'.

\(fn ATTR VALUE STREAMORBUFFER &optional SEARCH-PARTS SEARCH-INCLUDES)" nil nil)

(autoload 'semantic-brute-find-tag-by-function "semantic-find" "\
Find all tags for which FUNCTION's value is non-nil within STREAMORBUFFER.
FUNCTION must return non-nil if an element of STREAM will be included
in the new list.

If optional argument SEARCH-PARTS is non-nil, all sub-parts of tags
are searched.  The overloadable function `semantic-tag-componenets' is
used for the searching child lists.  If SEARCH-PARTS is the symbol
'positiononly, then only children that have positional information are
searched.

If SEARCH-INCLUDES has not been implemented.
This parameter hasn't be active for a while and is obsolete.

\(fn FUNCTION STREAMORBUFFER &optional SEARCH-PARTS SEARCH-INCLUDES)" nil nil)

(autoload 'semantic-brute-find-first-tag-by-function "semantic-find" "\
Find the first tag which FUNCTION match within STREAMORBUFFER.
FUNCTION must return non-nil if an element of STREAM will be included
in the new list.

The following parameters were never implemented.

If optional argument SEARCH-PARTS, all sub-parts of tags are searched.
The overloadable function `semantic-tag-components' is used for
searching.
If SEARCH-INCLUDES is non-nil, then all include files are also
searched for matches.

\(fn FUNCTION STREAMORBUFFER &optional SEARCH-PARTS SEARCH-INCLUDES)" nil nil)

(autoload 'semantic-brute-find-tag-by-position "semantic-find" "\
Find a tag covering POSITION within STREAMORBUFFER.
POSITION is a number, or marker.  If NOMEDIAN is non-nil, don't do
the median calculation, and return nil.

\(fn POSITION STREAMORBUFFER &optional NOMEDIAN)" nil nil)

(autoload 'semantic-brute-find-innermost-tag-by-position "semantic-find" "\
Find a list of tags covering POSITION within STREAMORBUFFER.
POSITION is a number, or marker.  If NOMEDIAN is non-nil, don't do
the median calculation, and return nil.
This function will find the topmost item, and recurse until no more
details are available of findable.

\(fn POSITION STREAMORBUFFER &optional NOMEDIAN)" nil nil)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-overlay 'semantic-find-tag-by-overlay)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-overlay-in-region 'semantic-find-tag-by-overlay-in-region)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-overlay-next 'semantic-find-tag-by-overlay-next)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-overlay-prev 'semantic-find-tag-by-overlay-prev)

(semantic-alias-obsolete 'semantic-find-nonterminal-parent-by-overlay 'semantic-find-tag-parent-by-overlay)

(semantic-alias-obsolete 'semantic-current-nonterminal 'semantic-current-tag)

(semantic-alias-obsolete 'semantic-current-nonterminal-parent 'semantic-current-tag-parent)

(semantic-alias-obsolete 'semantic-current-nonterminal-of-type 'semantic-current-tag-of-class)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-name 'semantic-brute-find-first-tag-by-name)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-token 'semantic-brute-find-tag-by-class)

(semantic-alias-obsolete 'semantic-find-nonterminal-standard 'semantic-brute-find-tag-standard)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-type 'semantic-brute-find-tag-by-type)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-type-regexp 'semantic-brute-find-tag-by-type-regexp)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-name-regexp 'semantic-brute-find-tag-by-name-regexp)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-property 'semantic-brute-find-tag-by-property)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-extra-spec 'semantic-brute-find-tag-by-attribute)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-extra-spec-value 'semantic-brute-find-tag-by-attribute-value)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-function 'semantic-brute-find-tag-by-function)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-function-first-match 'semantic-brute-find-first-tag-by-function)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-position 'semantic-brute-find-tag-by-position)

(semantic-alias-obsolete 'semantic-find-innermost-nonterminal-by-position 'semantic-brute-find-innermost-tag-by-position)

;;;***

;;;### (autoloads (semantic-folding-mode global-semantic-folding-mode
;;;;;;  global-semantic-folding-mode) "semantic-fold" "cedet/semantic/semantic-fold.el"
;;;;;;  (19390 35365))
;;; Generated autoloads from cedet/semantic/semantic-fold.el

(autoload 'global-semantic-folding-mode "semantic-fold" "\
Toggle global use of option `semantic-folding-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

(defvar global-semantic-folding-mode nil "\
*If non-nil enable global use of variable `semantic-folding-mode'.
With this mode enabled, a new folding decoration mode is added.
Clicking on a + or - in the fringe will fold that tag.")

(custom-autoload 'global-semantic-folding-mode "semantic-fold" nil)

(autoload 'semantic-folding-mode "semantic-fold" "\
Minor mode for highlighting changes made in a buffer.
Changes are tracked by semantic so that the incremental parser can work
properly.
This mode will display +/- icons in the fringe.  Clicking on them
will fold the current tag.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (semantic-format-tag-uml-concise-prototype semantic-format-tag-uml-prototype
;;;;;;  semantic-format-tag-uml-abbreviate semantic-format-tag-concise-prototype
;;;;;;  semantic-format-tag-prototype semantic-format-tag-short-doc
;;;;;;  semantic-format-tag-summarize-with-file semantic-format-tag-summarize
;;;;;;  semantic-format-tag-abbreviate semantic-format-tag-name semantic-format-tag-prin1
;;;;;;  semantic-format-tag-type) "semantic-format" "cedet/semantic/semantic-format.el"
;;;;;;  (19358 14566))
;;; Generated autoloads from cedet/semantic/semantic-format.el

(defvar semantic-format-tag-functions '(semantic-format-tag-name semantic-format-tag-canonical-name semantic-format-tag-abbreviate semantic-format-tag-summarize semantic-format-tag-summarize-with-file semantic-format-tag-short-doc semantic-format-tag-prototype semantic-format-tag-concise-prototype semantic-format-tag-uml-abbreviate semantic-format-tag-uml-prototype semantic-format-tag-uml-concise-prototype semantic-format-tag-prin1) "\
List of functions which convert a tag to text.
Each function must take the parameters TAG &optional PARENT COLOR.
TAG is the tag to convert.
PARENT is a parent tag or name which refers to the structure
or class which contains TAG.  PARENT is NOT a class which a TAG
would claim as a parent.
COLOR indicates that the generated text should be colored using
`font-lock'.")

(semantic-varalias-obsolete 'semantic-token->text-functions 'semantic-format-tag-functions)

(defvar semantic-format-tag-custom-list (append '(radio) (mapcar (lambda (f) (list 'const f)) semantic-format-tag-functions) '(function)) "\
A List used by customizable variables to choose a tag to text function.
Use this variable in the :type field of a customizable variable.")

(autoload 'semantic-format-tag-type "semantic-format" "\
Convert the data type of TAG to a string usable in tag formatting.
It is presumed that TYPE is a string or semantic tag.

\(fn TAG COLOR)" nil nil)

(autoload 'semantic-format-tag-prin1 "semantic-format" "\
Convert TAG to a string that is the print name for TAG.
PARENT and COLOR are ignored.

\(fn TAG &optional PARENT COLOR)" nil nil)

(autoload 'semantic-format-tag-name "semantic-format" "\
Return the name string describing TAG.
The name is the shortest possible representation.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.

\(fn TAG &optional PARENT COLOR)" nil nil)

(autoload 'semantic-format-tag-abbreviate "semantic-format" "\
Return an abbreviated string describing TAG.
The abbreviation is to be short, with possible symbols indicating
the type of tag, or other information.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.

\(fn TAG &optional PARENT COLOR)" nil nil)

(autoload 'semantic-format-tag-summarize "semantic-format" "\
Summarize TAG in a reasonable way.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.

\(fn TAG &optional PARENT COLOR)" nil nil)

(autoload 'semantic-format-tag-summarize-with-file "semantic-format" "\
Like `semantic-format-tag-summarize', but with the file name.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.

\(fn TAG &optional PARENT COLOR)" nil nil)

(autoload 'semantic-format-tag-short-doc "semantic-format" "\
Display a short form of TAG's documentation. (Comments, or docstring.)
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.

\(fn TAG &optional PARENT COLOR)" nil nil)

(autoload 'semantic-format-tag-prototype "semantic-format" "\
Return a prototype for TAG.
This function should be overloaded, though it need not be used.
This is because it can be used to create code by language independent
tools.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.

\(fn TAG &optional PARENT COLOR)" nil nil)

(autoload 'semantic-format-tag-concise-prototype "semantic-format" "\
Return a concise prototype for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.

\(fn TAG &optional PARENT COLOR)" nil nil)

(autoload 'semantic-format-tag-uml-abbreviate "semantic-format" "\
Return a UML style abbreviation for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.

\(fn TAG &optional PARENT COLOR)" nil nil)

(autoload 'semantic-format-tag-uml-prototype "semantic-format" "\
Return a UML style prototype for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.

\(fn TAG &optional PARENT COLOR)" nil nil)

(autoload 'semantic-format-tag-uml-concise-prototype "semantic-format" "\
Return a UML style concise prototype for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.

\(fn TAG &optional PARENT COLOR)" nil nil)

;;;***

;;;### (autoloads (semantic-gcc-test-output-parser-this-machine semantic-gcc-test-output-parser
;;;;;;  semantic-gcc-setup) "semantic-gcc" "cedet/semantic/bovine/semantic-gcc.el"
;;;;;;  (19358 14567))
;;; Generated autoloads from cedet/semantic/bovine/semantic-gcc.el

(autoload 'semantic-gcc-setup "semantic-gcc" "\
Setup Semantic C/C++ parsing based on GCC output.

\(fn)" t nil)

(autoload 'semantic-gcc-test-output-parser "semantic-gcc" "\
Test the output parser against some collected strings.

\(fn)" t nil)

(autoload 'semantic-gcc-test-output-parser-this-machine "semantic-gcc" "\
Test the output parser against the machine currently running Emacs.

\(fn)" t nil)

;;;***

;;;### (autoloads (semantic-grammar-batch-build-packages) "semantic-grammar"
;;;;;;  "cedet/semantic/semantic-grammar.el" (19565 48650))
;;; Generated autoloads from cedet/semantic/semantic-grammar.el

(autoload 'semantic-grammar-batch-build-packages "semantic-grammar" "\
Build Lisp packages from grammar files on the command line.
That is, run `semantic-grammar-batch-build-one-package' for each file.
Each file is processed even if an error occurred previously.
Must be used from the command line, with `-batch'.
For example, to process grammar files in current directory, invoke:

  \"emacs -batch -f semantic-grammar-batch-build-packages .\".

See also the variable `semantic-grammar-file-regexp'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (semantic-default-html-setup) "semantic-html" "cedet/semantic/semantic-html.el"
;;;;;;  (19358 14567))
;;; Generated autoloads from cedet/semantic/semantic-html.el

(autoload 'semantic-default-html-setup "semantic-html" "\
Set up a buffer for parsing of HTML files.

\(fn)" nil nil)

(add-hook 'html-mode-hook 'semantic-default-html-setup)

;;;***

;;;### (autoloads (semantic-ia-describe-class semantic-ia-show-doc
;;;;;;  semantic-ia-fast-mouse-jump semantic-ia-fast-jump semantic-ia-show-variants
;;;;;;  semantic-ia-show-summary semantic-ia-complete-tip semantic-ia-complete-symbol-menu
;;;;;;  semantic-ia-complete-symbol) "semantic-ia" "cedet/semantic/semantic-ia.el"
;;;;;;  (19546 10728))
;;; Generated autoloads from cedet/semantic/semantic-ia.el

(autoload 'semantic-ia-complete-symbol "semantic-ia" "\
Complete the current symbol at POS.
If POS is nil, default to point.
Completion options are calculated with `semantic-analyze-possible-completions'.

\(fn &optional POS)" t nil)

(autoload 'semantic-ia-complete-symbol-menu "semantic-ia" "\
Complete the current symbol via a menu based at POINT.
Completion options are calculated with `semantic-analyze-possible-completions'.

\(fn POINT)" t nil)

(autoload 'semantic-ia-complete-tip "semantic-ia" "\
Pop up a tooltip for completion at POINT.

\(fn POINT)" t nil)

(autoload 'semantic-ia-show-summary "semantic-ia" "\
Display a summary for the symbol under POINT.

\(fn POINT)" t nil)

(autoload 'semantic-ia-show-variants "semantic-ia" "\
Display a list of all variants for the symbol under POINT.

\(fn POINT)" t nil)

(autoload 'semantic-ia-fast-jump "semantic-ia" "\
Jump to the tag referred to by the code at POINT.
Uses `semantic-analyze-current-context' output to identify an accurate
origin of the code at point.

\(fn POINT)" t nil)

(autoload 'semantic-ia-fast-mouse-jump "semantic-ia" "\
Jump to the tag referred to by the point clicked on.
See `semantic-ia-fast-jump' for details on how it works.
 This command is meant to be bound to a mouse event.

\(fn EVT)" t nil)

(autoload 'semantic-ia-show-doc "semantic-ia" "\
Display the code-level documentation for the symbol at POINT.

\(fn POINT)" t nil)

(autoload 'semantic-ia-describe-class "semantic-ia" "\
Display all known parts for the datatype TYPENAME.
If the type in question is a class, all methods and other accessible
parts of the parent classes are displayed.

\(fn TYPENAME)" t nil)

;;;***

;;;### (autoloads (semantic-speedbar-analysis) "semantic-ia-sb" "cedet/semantic/semantic-ia-sb.el"
;;;;;;  (19358 14567))
;;; Generated autoloads from cedet/semantic/semantic-ia-sb.el

(autoload 'semantic-speedbar-analysis "semantic-ia-sb" "\
Start Speedbar in semantic analysis mode.
The analyzer displays information about the current context, plus a smart
list of possible completions.

\(fn)" t nil)

;;;***

;;;### (autoloads (semantic-ia-utest) "semantic-ia-utest" "cedet/semantic/semantic-ia-utest.el"
;;;;;;  (19546 10763))
;;; Generated autoloads from cedet/semantic/semantic-ia-utest.el

(autoload 'semantic-ia-utest "semantic-ia-utest" "\
Run the semantic ia unit test against stored sources.
Argument ARG specifies which set of tests to run.
 1 - ia utests
 2 - regs utests
 3 - symrefs utests
 4 - symref count utests

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (global-semantic-idle-scheduler-mode semantic-idle-scheduler-remove
;;;;;;  semantic-idle-scheduler-add semantic-idle-scheduler-mode
;;;;;;  global-semantic-idle-scheduler-mode) "semantic-idle" "cedet/semantic/semantic-idle.el"
;;;;;;  (19447 4521))
;;; Generated autoloads from cedet/semantic/semantic-idle.el

(defvar global-semantic-idle-scheduler-mode nil "\
*If non-nil, enable global use of idle-scheduler mode.")

(custom-autoload 'global-semantic-idle-scheduler-mode "semantic-idle" nil)

(defvar semantic-idle-scheduler-mode nil "\
Non-nil if idle-scheduler minor mode is enabled.
Use the command `semantic-idle-scheduler-mode' to change this variable.")

(autoload 'semantic-idle-scheduler-mode "semantic-idle" "\
Minor mode to auto parse buffer following a change.
When this mode is off, a buffer is only rescanned for tokens when
some command requests the list of available tokens.  When idle-scheduler
is enabled, Emacs periodically checks to see if the buffer is out of
date, and reparses while the user is idle (not typing.)

With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\(fn &optional ARG)" t nil)

(autoload 'semantic-idle-scheduler-add "semantic-idle" "\
Schedule FUNCTION to occur during idle time.

\(fn FUNCTION)" nil nil)

(autoload 'semantic-idle-scheduler-remove "semantic-idle" "\
Unschedule FUNCTION to occur during idle time.

\(fn FUNCTION)" nil nil)

(autoload 'global-semantic-idle-scheduler-mode "semantic-idle" "\
Toggle global use of option `semantic-idle-scheduler-mode'.
The idle scheduler will automatically reparse buffers in idle time,
and then schedule other jobs setup with `semantic-idle-scheduler-add'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (semantic-create-imenu-index semantic-imenu-expand-type-members
;;;;;;  semantic-imenu-bucketize-file semantic-imenu-summary-function)
;;;;;;  "semantic-imenu" "cedet/semantic/semantic-imenu.el" (19530
;;;;;;  54442))
;;; Generated autoloads from cedet/semantic/semantic-imenu.el

(defvar semantic-imenu-summary-function 'semantic-format-tag-abbreviate "\
*Function to use when creating items in Imenu.
Some useful functions are found in `semantic-format-tag-functions'.")

(custom-autoload 'semantic-imenu-summary-function "semantic-imenu" t)

(defvar semantic-imenu-bucketize-file t "\
*Non-nil if tags in a file are to be grouped into buckets.")

(custom-autoload 'semantic-imenu-bucketize-file "semantic-imenu" t)

(defvar semantic-imenu-expand-type-members t "\
*Non-nil if types should have submenus with members in them.")

(custom-autoload 'semantic-imenu-expand-type-members "semantic-imenu" t)

(defvar semantic-imenu-expandable-tag-classes '(type) "\
List of expandable tag classes.
Tags of those classes will be given submenu with children.
By default, a `type' has interesting children.  In Texinfo, however, a
`section' has interesting children.")

(autoload 'semantic-create-imenu-index "semantic-imenu" "\
Create an imenu index for any buffer which supports Semantic.
Uses the output of the Semantic parser to create the index.
Optional argument STREAM is an optional stream of tags used to create menus.

\(fn &optional STREAM)" nil nil)

;;;***

;;;### (autoloads (define-lex-block-analyzer define-lex-simple-regex-analyzer
;;;;;;  define-lex-regex-analyzer define-lex-analyzer semantic-lex
;;;;;;  semantic-lex-init define-lex) "semantic-lex" "cedet/semantic/semantic-lex.el"
;;;;;;  (19373 12955))
;;; Generated autoloads from cedet/semantic/semantic-lex.el

(defvar semantic-lex-analyzer 'semantic-flex "\
The lexical analyzer used for a given buffer.
See `semantic-lex' for documentation.
For compatibility with Semantic 1.x it defaults to `semantic-flex'.")

(autoload 'define-lex "semantic-lex" "\
Create a new lexical analyzer with NAME.
DOC is a documentation string describing this analyzer.
ANALYZERS are small code snippets of analyzers to use when
building the new NAMED analyzer.  Only use analyzers which
are written to be used in `define-lex'.
Each analyzer should be an analyzer created with `define-lex-analyzer'.
Note: The order in which analyzers are listed is important.
If two analyzers can match the same text, it is important to order the
analyzers so that the one you want to match first occurs first.  For
example, it is good to put a number analyzer in front of a symbol
analyzer which might mistake a number for as a symbol.

\(fn NAME DOC &rest ANALYZERS)" nil (quote macro))

(autoload 'semantic-lex-init "semantic-lex" "\
Initialize any lexical state for this buffer.

\(fn)" nil nil)

(autoload 'semantic-lex "semantic-lex" "\
Lexically analyze text in the current buffer between START and END.
Optional argument DEPTH indicates at what level to scan over entire
lists.  The last argument, LENGTH specifies that `semantic-lex'
should only return LENGTH tokens.  The return value is a token stream.
Each element is a list, such of the form
  (symbol start-expression .  end-expression)
where SYMBOL denotes the token type.
See `semantic-lex-tokens' variable for details on token types.  END
does not mark the end of the text scanned, only the end of the
beginning of text scanned.  Thus, if a string extends past END, the
end of the return token will be larger than END.  To truly restrict
scanning, use `narrow-to-region'.

\(fn START END &optional DEPTH LENGTH)" nil nil)

(autoload 'define-lex-analyzer "semantic-lex" "\
Create a single lexical analyzer NAME with DOC.
When an analyzer is called, the current buffer and point are
positioned in a buffer at the location to be analyzed.
CONDITION is an expression which returns t if FORMS should be run.
Within the bounds of CONDITION and FORMS, the use of backquote
can be used to evaluate expressions at compile time.
While forms are running, the following variables will be locally bound:
  `semantic-lex-analysis-bounds' - The bounds of the current analysis.
                  of the form (START . END)
  `semantic-lex-maximum-depth' - The maximum depth of semantic-list
                  for the current analysis.
  `semantic-lex-current-depth' - The current depth of `semantic-list' that has
                  been descended.
  `semantic-lex-end-point' - End Point after match.
                   Analyzers should set this to a buffer location if their
                   match string does not represent the end of the matched text.
  `semantic-lex-token-stream' - The token list being collected.
                   Add new lexical tokens to this list.
Proper action in FORMS is to move the value of `semantic-lex-end-point' to
after the location of the analyzed entry, and to add any discovered tokens
at the beginning of `semantic-lex-token-stream'.
This can be done by using `semantic-lex-push-token'.

\(fn NAME DOC CONDITION &rest FORMS)" nil (quote macro))

(autoload 'define-lex-regex-analyzer "semantic-lex" "\
Create a lexical analyzer with NAME and DOC that will match REGEXP.
FORMS are evaluated upon a successful match.
See `define-lex-analyzer' for more about analyzers.

\(fn NAME DOC REGEXP &rest FORMS)" nil (quote macro))

(autoload 'define-lex-simple-regex-analyzer "semantic-lex" "\
Create a lexical analyzer with NAME and DOC that match REGEXP.
TOKSYM is the symbol to use when creating a semantic lexical token.
INDEX is the index into the match that defines the bounds of the token.
Index should be a plain integer, and not specified in the macro as an
expression.
FORMS are evaluated upon a successful match BEFORE the new token is
created.  It is valid to ignore FORMS.
See `define-lex-analyzer' for more about analyzers.

\(fn NAME DOC REGEXP TOKSYM &optional INDEX &rest FORMS)" nil (quote macro))

(autoload 'define-lex-block-analyzer "semantic-lex" "\
Create a lexical analyzer NAME for paired delimiters blocks.
It detects a paired delimiters block or the corresponding open or
close delimiter depending on the value of the variable
`semantic-lex-current-depth'.  DOC is the documentation string of the lexical
analyzer.  SPEC1 and SPECS specify the token symbols and open, close
delimiters used.  Each SPEC has the form:

\(BLOCK-SYM (OPEN-DELIM OPEN-SYM) (CLOSE-DELIM CLOSE-SYM))

where BLOCK-SYM is the symbol returned in a block token.  OPEN-DELIM
and CLOSE-DELIM are respectively the open and close delimiters
identifying a block.  OPEN-SYM and CLOSE-SYM are respectively the
symbols returned in open and close tokens.

\(fn NAME DOC SPEC1 &rest SPECS)" nil (quote macro))

;;;***

;;;### (autoloads (semantic-lex-spp-write-utest semantic-lex-spp-table-write-slot-value)
;;;;;;  "semantic-lex-spp" "cedet/semantic/semantic-lex-spp.el" (19403
;;;;;;  28207))
;;; Generated autoloads from cedet/semantic/semantic-lex-spp.el

(autoload 'semantic-lex-spp-table-write-slot-value "semantic-lex-spp" "\
Write out the VALUE of a slot for EIEIO.
The VALUE is a spp lexical table.

\(fn VALUE)" nil nil)

(autoload 'semantic-lex-spp-write-utest "semantic-lex-spp" "\
Unit test using the test spp file to test the slot write fcn.

\(fn)" t nil)

;;;***

;;;### (autoloads (semantic-load-enable-all-exuberent-ctags-support)
;;;;;;  "semantic-load" "cedet/semantic/semantic-load.el" (19404
;;;;;;  63607))
;;; Generated autoloads from cedet/semantic/semantic-load.el

(autoload 'semantic-load-enable-all-exuberent-ctags-support "semantic-load" "\
Enable all exuberent ctags extensions.
See the functions:
   `semantic-load-enable-primary-exuberent-ctags-support'
   `semantic-load-enable-secondary-exuberent-ctags-support'
If you just want to add new languages, use
   `semantic-load-enable-primary-exuberent-ctags-support'.

\(fn)" t nil)

;;;***

;;;### (autoloads (semantic-default-make-setup) "semantic-make" "cedet/semantic/bovine/semantic-make.el"
;;;;;;  (19358 14567))
;;; Generated autoloads from cedet/semantic/bovine/semantic-make.el

(autoload 'semantic-default-make-setup "semantic-make" "\
Set up a Makefile buffer for parsing with semantic.

\(fn)" nil nil)

(add-hook 'makefile-mode-hook 'semantic-default-make-setup)

;;;***

;;;### (autoloads (semantic-mru-bookmark-mode global-semantic-mru-bookmark-mode
;;;;;;  global-semantic-mru-bookmark-mode) "semantic-mru-bookmark"
;;;;;;  "cedet/semantic/semantic-mru-bookmark.el" (19390 35041))
;;; Generated autoloads from cedet/semantic/semantic-mru-bookmark.el

(autoload 'global-semantic-mru-bookmark-mode "semantic-mru-bookmark" "\
Toggle global use of option `semantic-mru-bookmark-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

(defvar global-semantic-mru-bookmark-mode nil "\
If non-nil, enable `semantic-mru-bookmark-mode' globally.
When this mode is enabled, Emacs keeps track of which tags have
been edited, and you can re-visit them with \\[semantic-mrub-switch-tags].")

(custom-autoload 'global-semantic-mru-bookmark-mode "semantic-mru-bookmark" nil)

(autoload 'semantic-mru-bookmark-mode "semantic-mru-bookmark" "\
Minor mode for tracking tag-based bookmarks automatically.
When this mode is enabled, Emacs keeps track of which tags have
been edited, and you can re-visit them with \\[semantic-mrub-switch-tags].

\\{semantic-mru-bookmark-mode-map}

With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (semantic-regtest-cmp-results semantic-regtest-create-output
;;;;;;  semantic-regtest-run-test) "semantic-regtest" "cedet/semantic/semantic-regtest.el"
;;;;;;  (17213 40495))
;;; Generated autoloads from cedet/semantic/semantic-regtest.el

(autoload 'semantic-regtest-run-test "semantic-regtest" "\
Not documented

\(fn)" t nil)

(autoload 'semantic-regtest-create-output "semantic-regtest" "\
Creates the test-output for the current buffer.
The user will be asked for the file-name of the created test-output-file (see
`semantic-regtest-create-output--internal').

\(fn)" t nil)

(autoload 'semantic-regtest-cmp-results "semantic-regtest" "\
Compare two test-outputs and create a suitable formatted result-file.

The user will be asked for four file-names:

   SOURCE-FILE: The underlying source-file for which the test-outputs have
   been created. If current buffer is a semantic-supported buffer then the
   file-name of the current buffer is offered as default.

   TEST-FILE: The regression-testoutput for SOURCE-FILE. It must be an already
   existing file which has been created by `semantic-regtest-create-output' or
   the function `semantic-regtest-create-output--internal'. If a file
   SOURCE-FILE.to exists already in current directory then this file is
   offered as default.

   REF-FILE: The reference testoutput for SOURCE-FILE. TEST-FILE will be
   compared against this file. It must be an already existing file which has
   been created by the command `semantic-regtest-create-output' or the
   function `semantic-regtest-create-output--internal'. If a file
   SOURCE-FILE.ro exists already in current directory then this file is
   offered as default.

   RESULT-FILE: That file will contain the comparisson-result generated by
   `semantic-regtest-cmp-results--internal'. Per default the filename
   SOURCE-FILE.res is offered.

This command calls `semantic-regtest-cmp-results--internal' with that four
file-names. See this function for details about the optional argument
`use-full-path-name' and a description of the format of RESULT-FILE.

\(fn &optional USE-FULL-PATH-NAME)" t nil)

;;;***

;;;### (autoloads (semantic-default-scheme-setup) "semantic-scm"
;;;;;;  "cedet/semantic/bovine/semantic-scm.el" (18955 30432))
;;; Generated autoloads from cedet/semantic/bovine/semantic-scm.el

(autoload 'semantic-default-scheme-setup "semantic-scm" "\
Setup hook function for Emacs Lisp files and Semantic.

\(fn)" nil nil)

(add-hook 'scheme-mode-hook 'semantic-default-scheme-setup)

;;;***

;;;### (autoloads (semantic-calculate-scope semantic-scope-tag-clone-with-scope
;;;;;;  semantic-scope-reset-cache) "semantic-scope" "cedet/semantic/semantic-scope.el"
;;;;;;  (19393 7266))
;;; Generated autoloads from cedet/semantic/semantic-scope.el

(autoload 'semantic-scope-reset-cache "semantic-scope" "\
Get the current cached scope, and reset it.

\(fn)" nil nil)

(autoload 'semantic-scope-tag-clone-with-scope "semantic-scope" "\
Close TAG, and return it.  Add SCOPETAGS as a tag-local scope.
Stores the SCOPETAGS as a set of tag properties on the cloned tag.

\(fn TAG SCOPETAGS)" nil nil)

(autoload 'semantic-calculate-scope "semantic-scope" "\
Calculate the scope at POINT.
If POINT is not provided, then use the current location of point.
The class returned from the scope calculation is variable
`semantic-scope-cache'.

\(fn &optional POINT)" t nil)

;;;***

;;;### (autoloads (semantic-default-skel-setup) "semantic-skel" "cedet/semantic/bovine/semantic-skel.el"
;;;;;;  (19358 14567))
;;; Generated autoloads from cedet/semantic/bovine/semantic-skel.el

(autoload 'semantic-default-skel-setup "semantic-skel" "\
Set up a buffer for semantic parsing of the skeleton language.

\(fn)" nil nil)

;;;***

;;;### (autoloads (semantic-tag-external-class semantic-tag-external-member-children
;;;;;;  semantic-tag-external-member-p semantic-tag-external-member-parent
;;;;;;  semantic-adopt-external-members semantic-bucketize semantic-flatten-tags-table
;;;;;;  semantic-unique-tag-table semantic-unique-tag-table-by-name
;;;;;;  semantic-sort-tags-by-name-then-type-decreasing semantic-sort-tags-by-name-then-type-increasing
;;;;;;  semantic-sort-tags-by-type-decreasing-ci semantic-sort-tags-by-type-increasing-ci
;;;;;;  semantic-sort-tags-by-name-decreasing-ci semantic-sort-tags-by-name-increasing-ci
;;;;;;  semantic-sort-tags-by-type-decreasing semantic-sort-tags-by-type-increasing
;;;;;;  semantic-sort-tags-by-name-decreasing semantic-sort-tags-by-name-increasing)
;;;;;;  "semantic-sort" "cedet/semantic/semantic-sort.el" (19269
;;;;;;  17811))
;;; Generated autoloads from cedet/semantic/semantic-sort.el

(autoload 'semantic-sort-tags-by-name-increasing "semantic-sort" "\
Sort TAGS by name in increasing order with side effects.
Return the sorted list.

\(fn TAGS)" nil nil)

(autoload 'semantic-sort-tags-by-name-decreasing "semantic-sort" "\
Sort TAGS by name in decreasing order with side effects.
Return the sorted list.

\(fn TAGS)" nil nil)

(autoload 'semantic-sort-tags-by-type-increasing "semantic-sort" "\
Sort TAGS by type in increasing order with side effects.
Return the sorted list.

\(fn TAGS)" nil nil)

(autoload 'semantic-sort-tags-by-type-decreasing "semantic-sort" "\
Sort TAGS by type in decreasing order with side effects.
Return the sorted list.

\(fn TAGS)" nil nil)

(autoload 'semantic-sort-tags-by-name-increasing-ci "semantic-sort" "\
Sort TAGS by name in increasing order with side effects.
Return the sorted list.

\(fn TAGS)" nil nil)

(autoload 'semantic-sort-tags-by-name-decreasing-ci "semantic-sort" "\
Sort TAGS by name in decreasing order with side effects.
Return the sorted list.

\(fn TAGS)" nil nil)

(autoload 'semantic-sort-tags-by-type-increasing-ci "semantic-sort" "\
Sort TAGS by type in increasing order with side effects.
Return the sorted list.

\(fn TAGS)" nil nil)

(autoload 'semantic-sort-tags-by-type-decreasing-ci "semantic-sort" "\
Sort TAGS by type in decreasing order with side effects.
Return the sorted list.

\(fn TAGS)" nil nil)

(autoload 'semantic-sort-tags-by-name-then-type-increasing "semantic-sort" "\
Sort TAGS by name, then type in increasing order with side effects.
Return the sorted list.

\(fn TAGS)" nil nil)

(autoload 'semantic-sort-tags-by-name-then-type-decreasing "semantic-sort" "\
Sort TAGS by name, then type in increasing order with side effects.
Return the sorted list.

\(fn TAGS)" nil nil)

(autoload 'semantic-unique-tag-table-by-name "semantic-sort" "\
Scan a list of TAGS, removing duplicate names.
This must first sort the tags by name alphabetically ascending.
For more complex uniqueness testing used by the semanticdb
typecaching system, see `semanticdb-typecache-merge-streams'.

\(fn TAGS)" nil nil)

(autoload 'semantic-unique-tag-table "semantic-sort" "\
Scan a list of TAGS, removing duplicates.
This must first sort the tags by position ascending.
TAGS are removed only if they are equivalent, as can happen when
multiple tag sources are scanned.
For more complex uniqueness testing used by the semanticdb
typecaching system, see `semanticdb-typecache-merge-streams'.

\(fn TAGS)" nil nil)

(autoload 'semantic-flatten-tags-table "semantic-sort" "\
Flatten the tags table TABLE.
All tags in TABLE, and all components of top level tags
in TABLE will appear at the top level of list.
Tags promoted to the top of the list will still appear
unmodified as components of their parent tags.

\(fn &optional TABLE)" nil nil)

(autoload 'semantic-bucketize "semantic-sort" "\
Sort TAGS into a group of buckets based on tag class.
Unknown classes are placed in a Misc bucket.
Type bucket names are defined by either `semantic-symbol->name-assoc-list'.
If PARENT is specified, then TAGS belong to this PARENT in some way.
This will use `semantic-symbol->name-assoc-list-for-type-parts' to
generate bucket names.
Optional argument FILTER is a filter function to be applied to each bucket.
The filter function will take one argument, which is a list of tokens, and
may re-organize the list with side-effects.

\(fn TAGS &optional PARENT FILTER)" nil nil)

(defvar semantic-orphaned-member-metaparent-type "class" "\
In `semantic-adopt-external-members', the type of 'type for metaparents.
A metaparent is a made-up type semantic token used to hold the child list
of orphaned members of a named type.")

(autoload 'semantic-adopt-external-members "semantic-sort" "\
Rebuild TAGS so that externally defined members are regrouped.
Some languages such as C++ and CLOS permit the declaration of member
functions outside the definition of the class.  It is easier to study
the structure of a program when such methods are grouped together
more logically.

This function uses `semantic-tag-external-member-p' to
determine when a potential child is an externally defined member.

Note: Applications which use this function must account for token
types which do not have a position, but have children which *do*
have positions.

Applications should use `semantic-mark-external-member-function'
to modify all tags which are found as externally defined to some
type.  For example, changing the token type for generating extra
buckets with the bucket function.

\(fn TAGS)" nil nil)

(autoload 'semantic-tag-external-member-parent "semantic-sort" "\
Return a parent for TAG when TAG is an external member.
TAG is an external member if it is defined at a toplevel and
has some sort of label defining a parent.  The parent return will
be a string.

The default behavior, if not overridden with
`tag-member-parent' gets the 'parent extra
specifier of TAG.

If this function is overridden, use
`semantic-tag-external-member-parent-default' to also
include the default behavior, and merely extend your own.

\(fn TAG)" nil nil)

(autoload 'semantic-tag-external-member-p "semantic-sort" "\
Return non-nil if PARENT is the parent of TAG.
TAG is an external member of PARENT when it is somehow tagged
as having PARENT as its parent.
PARENT and TAG must both be semantic tags.

The default behavior, if not overridden with
`tag-external-member-p' is to match :parent attribute in
the name of TAG.

If this function is overridden, use
`semantic-tag-external-member-children-p-default' to also
include the default behavior, and merely extend your own.

\(fn PARENT TAG)" nil nil)

(autoload 'semantic-tag-external-member-children "semantic-sort" "\
Return the list of children which are not *in* TAG.
If optional argument USEDB is non-nil, then also search files in
the Semantic Database.  If USEDB is a list of databases, search those
databases.

Children in this case are functions or types which are members of
TAG, such as the parts of a type, but which are not defined inside
the class.  C++ and CLOS both permit methods of a class to be defined
outside the bounds of the class' definition.

The default behavior, if not overridden with
`tag-external-member-children' is to search using
`semantic-tag-external-member-p' in all top level definitions
with a parent of TAG.

If this function is overridden, use
`semantic-tag-external-member-children-default' to also
include the default behavior, and merely extend your own.

\(fn TAG &optional USEDB)" nil nil)

(autoload 'semantic-tag-external-class "semantic-sort" "\
Return a list of real tags that faux TAG might represent.

In some languages, a method can be defined on an object which is
not in the same file.  In this case,
`semantic-adopt-external-members' will create a faux-tag.  If it
is necessary to get the tag from which for faux TAG was most
likely derived, then this function is needed.

\(fn TAG)" nil nil)

;;;***

;;;### (autoloads (semantic-symref-find-text semantic-symref-find-file-references-by-name
;;;;;;  semantic-symref-find-tags-by-completion semantic-symref-find-tags-by-regexp
;;;;;;  semantic-symref-find-tags-by-name semantic-symref-find-references-by-name)
;;;;;;  "semantic-symref" "cedet/semantic/symref/semantic-symref.el"
;;;;;;  (19390 35306))
;;; Generated autoloads from cedet/semantic/symref/semantic-symref.el

(autoload 'semantic-symref-find-references-by-name "semantic-symref" "\
Find a list of references to NAME in the current project.
Optional SCOPE specifies which file set to search.  Defaults to 'project.
Refers to `semantic-symref-tool', to determine the reference tool to use
for the current buffer.
Returns an object of class `semantic-symref-result'.
TOOL-RETURN is an optional symbol, which will be assigned the tool used
to perform the search.  This was added for use by a test harness.

\(fn NAME &optional SCOPE TOOL-RETURN)" t nil)

(autoload 'semantic-symref-find-tags-by-name "semantic-symref" "\
Find a list of references to NAME in the current project.
Optional SCOPE specifies which file set to search.  Defaults to 'project.
Refers to `semantic-symref-tool', to determine the reference tool to use
for the current buffer.
Returns an object of class `semantic-symref-result'.

\(fn NAME &optional SCOPE)" t nil)

(autoload 'semantic-symref-find-tags-by-regexp "semantic-symref" "\
Find a list of references to NAME in the current project.
Optional SCOPE specifies which file set to search.  Defaults to 'project.
Refers to `semantic-symref-tool', to determine the reference tool to use
for the current buffer.
Returns an object of class `semantic-symref-result'.

\(fn NAME &optional SCOPE)" t nil)

(autoload 'semantic-symref-find-tags-by-completion "semantic-symref" "\
Find a list of references to NAME in the current project.
Optional SCOPE specifies which file set to search.  Defaults to 'project.
Refers to `semantic-symref-tool', to determine the reference tool to use
for the current buffer.
Returns an object of class `semantic-symref-result'.

\(fn NAME &optional SCOPE)" t nil)

(autoload 'semantic-symref-find-file-references-by-name "semantic-symref" "\
Find a list of references to NAME in the current project.
Optional SCOPE specifies which file set to search.  Defaults to 'project.
Refers to `semantic-symref-tool', to determine the reference tool to use
for the current buffer.
Returns an object of class `semantic-symref-result'.

\(fn NAME &optional SCOPE)" t nil)

(autoload 'semantic-symref-find-text "semantic-symref" "\
Find a list of occurrences of TEXT in the current project.
TEXT is a regexp formatted for use with egrep.
Optional SCOPE specifies which file set to search.  Defaults to 'project.
Refers to `semantic-symref-tool', to determine the reference tool to use
for the current buffer.
Returns an object of class `semantic-symref-result'.

\(fn TEXT &optional SCOPE)" t nil)

;;;***

;;;### (autoloads nil "semantic-symref-cscope" "cedet/semantic/symref/semantic-symref-cscope.el"
;;;;;;  (18855 29150))
;;; Generated autoloads from cedet/semantic/symref/semantic-symref-cscope.el

(eieio-defclass-autoload 'semantic-symref-tool-cscope '(semantic-symref-tool-baseclass) "semantic-symref-cscope" "A symref tool implementation using CScope.\nThe CScope command can be used to generate lists of tags in a way\nsimilar to that of `grep'.  This tool will parse the output to generate\nthe hit list.\n\nSee the function `cedet-cscope-search' for more details.")

;;;***

;;;### (autoloads (semantic-symref-rename-local-variable semantic-symref-test-count-hits-in-tag
;;;;;;  semantic-symref-hits-in-region) "semantic-symref-filter"
;;;;;;  "cedet/semantic/symref/semantic-symref-filter.el" (19390
;;;;;;  35169))
;;; Generated autoloads from cedet/semantic/symref/semantic-symref-filter.el

(autoload 'semantic-symref-hits-in-region "semantic-symref-filter" "\
Find all occurrences of the symbol TARGET that match TARGET the tag.
For each match, call HOOKFCN.
HOOKFCN takes three arguments that match
`semantic-analyze-current-symbol's use of HOOKFCN.
  ( START END PREFIX )

Search occurs in the current buffer between START and END.

\(fn TARGET HOOKFCN START END)" nil nil)

(autoload 'semantic-symref-test-count-hits-in-tag "semantic-symref-filter" "\
Lookup in the current tag the symbol under point.
the count all the other references to the same symbol within the
tag that contains point, and return that.

\(fn)" t nil)

(autoload 'semantic-symref-rename-local-variable "semantic-symref-filter" "\
Fancy way to rename the local variable under point.
Depends on the SRecode Field editing API.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "semantic-symref-global" "cedet/semantic/symref/semantic-symref-global.el"
;;;;;;  (18855 29540))
;;; Generated autoloads from cedet/semantic/symref/semantic-symref-global.el

(eieio-defclass-autoload 'semantic-symref-tool-global '(semantic-symref-tool-baseclass) "semantic-symref-global" "A symref tool implementation using GNU Global.\nThe GNU Global command can be used to generate lists of tags in a way\nsimilar to that of `grep'.  This tool will parse the output to generate\nthe hit list.\n\nSee the function `cedet-gnu-global-search' for more details.")

;;;***

;;;### (autoloads nil "semantic-symref-grep" "cedet/semantic/symref/semantic-symref-grep.el"
;;;;;;  (19422 9029))
;;; Generated autoloads from cedet/semantic/symref/semantic-symref-grep.el

(eieio-defclass-autoload 'semantic-symref-tool-grep '(semantic-symref-tool-baseclass) "semantic-symref-grep" "A symref tool implementation using grep.\nThis tool uses EDE to find he root of the project, then executes\nfind-grep in the project.  The output is parsed for hits\nand those hits returned.")

;;;***

;;;### (autoloads nil "semantic-symref-idutils" "cedet/semantic/symref/semantic-symref-idutils.el"
;;;;;;  (18852 3236))
;;; Generated autoloads from cedet/semantic/symref/semantic-symref-idutils.el

(eieio-defclass-autoload 'semantic-symref-tool-idutils '(semantic-symref-tool-baseclass) "semantic-symref-idutils" "A symref tool implementation using ID Utils.\nThe udutils command set can be used to generate lists of tags in a way\nsimilar to that of `grep'.  This tool will parse the output to generate\nthe hit list.\n\nSee the function `cedet-idutils-search' for more details.")

;;;***

;;;### (autoloads (semantic-symref-results-mode semantic-symref-regexp
;;;;;;  semantic-symref-symbol semantic-symref) "semantic-symref-list"
;;;;;;  "cedet/semantic/symref/semantic-symref-list.el" (19476 12715))
;;; Generated autoloads from cedet/semantic/symref/semantic-symref-list.el

(autoload 'semantic-symref "semantic-symref-list" "\
Find references to the current tag.
This command uses the currently configured references tool within the
current project to find references to the current tag.  The
references are the organized by file and the name of the function
they are used in.
Display the references in`semantic-symref-results-mode'.

\(fn)" t nil)

(autoload 'semantic-symref-symbol "semantic-symref-list" "\
Find references to the symbol SYM.
This command uses the currently configured references tool within the
current project to find references to the input SYM.  The
references are the organized by file and the name of the function
they are used in.
Display the references in`semantic-symref-results-mode'.

\(fn SYM)" t nil)

(autoload 'semantic-symref-regexp "semantic-symref-list" "\
Find references to the a symbol regexp SYM.
This command uses the currently configured references tool within the
current project to find references to the input SYM.  The
references are the organized by file and the name of the function
they are used in.
Display the references in`semantic-symref-results-mode'.

\(fn SYM)" t nil)

(autoload 'semantic-symref-results-mode "semantic-symref-list" "\
Major-mode for displaying Semantic Symbol Reference RESULTS.
RESULTS is an object of class `semantic-symref-results'.

\(fn RESULTS)" t nil)

;;;***

;;;### (autoloads (semantic-insert-foreign-tag semantic-obtain-foreign-tag
;;;;;;  semantic-tag-components-with-overlays semantic-tag-components
;;;;;;  semantic-tag-alias-definition) "semantic-tag" "cedet/semantic/semantic-tag.el"
;;;;;;  (19358 14567))
;;; Generated autoloads from cedet/semantic/semantic-tag.el

(defsubst semantic-tag-p (tag) "\
Return non-nil if TAG is most likely a semantic tag." (condition-case nil (and (consp tag) (stringp (car tag)) (symbolp (nth 1 tag)) (nth 1 tag) (listp (nth 2 tag)) (listp (nth 3 tag))) (error nil)))

(autoload 'semantic-tag-alias-definition "semantic-tag" "\
Return the definition TAG is an alias.
The returned value is a tag of the class that
`semantic-tag-alias-class' returns for TAG.
The default is to return the value of the :definition attribute.
Return nil if TAG is not of class 'alias.

\(fn TAG)" nil nil)

(autoload 'semantic-tag-components "semantic-tag" "\
Return a list of components for TAG.
A Component is a part of TAG which itself may be a TAG.
Examples include the elements of a structure in a
tag of class `type, or the list of arguments to a
tag of class 'function.

\(fn TAG)" nil nil)

(autoload 'semantic-tag-components-with-overlays "semantic-tag" "\
Return the list of top level components belonging to TAG.
Children are any sub-tags which contain overlays.

Default behavior is to get `semantic-tag-components' in addition
to the components of an anonymous types (if applicable.)

Note for language authors:
  If a mode defines a language tag that has tags in it with overlays
you should still return them with this function.
Ignoring this step will prevent several features from working correctly.

\(fn TAG)" nil nil)

(autoload 'semantic-obtain-foreign-tag "semantic-tag" "\
Obtain a foreign tag from TAG.
TAG defaults to the tag at point in current buffer.
Return the obtained foreign tag or nil if failed.

\(fn &optional TAG)" nil nil)

(autoload 'semantic-insert-foreign-tag "semantic-tag" "\
Insert FOREIGN-TAG into the current buffer.
Signal an error if FOREIGN-TAG is not a valid foreign tag.
This function is overridable with the symbol `insert-foreign-tag'.

\(fn FOREIGN-TAG)" nil nil)

;;;***

;;;### (autoloads (semantic-prototype-file semantic-dependency-tag-file
;;;;;;  semantic-go-to-tag) "semantic-tag-file" "cedet/semantic/semantic-tag-file.el"
;;;;;;  (19358 14567))
;;; Generated autoloads from cedet/semantic/semantic-tag-file.el

(autoload 'semantic-go-to-tag "semantic-tag-file" "\
Go to the location of TAG.
TAG may be a stripped element, in which case PARENT specifies a
parent tag that has position information.
PARENT can also be a `semanticdb-table' object.

\(fn TAG &optional PARENT)" nil nil)

(autoload 'semantic-dependency-tag-file "semantic-tag-file" "\
Find the filename represented from TAG.
Depends on `semantic-dependency-include-path' for searching.  Always searches
`.' first, then searches additional paths.

\(fn &optional TAG)" nil nil)

(autoload 'semantic-prototype-file "semantic-tag-file" "\
Return a file in which prototypes belonging to BUFFER should be placed.
Default behavior (if not overridden) looks for a token specifying the
prototype file, or the existence of an EDE variable indicating which
file prototypes belong in.

\(fn BUFFER)" nil nil)

;;;***

;;;### (autoloads (semantic-tag-folding-mode global-semantic-tag-folding-mode
;;;;;;  global-semantic-tag-folding-mode) "semantic-tag-folding"
;;;;;;  "cedet/contrib/semantic-tag-folding.el" (19390 36084))
;;; Generated autoloads from cedet/contrib/semantic-tag-folding.el

(defvar global-semantic-tag-folding-mode nil "\
*If non-nil enable global use of variable `semantic-tag-folding-mode'.
With this mode enabled, a new folding decoration mode is added.
Clicking on a + or - in the fringe will fold that tag.")

(custom-autoload 'global-semantic-tag-folding-mode "semantic-tag-folding" nil)

(autoload 'global-semantic-tag-folding-mode "semantic-tag-folding" "\
Toggle global use of option `semantic-tag-folding-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

(autoload 'semantic-tag-folding-mode "semantic-tag-folding" "\
Minor mode mark semantic tags for folding.
This mode will display +/- icons in the fringe.  Clicking on them
will fold the current tag.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (semantic-tag-full-name semantic-tag-prototype-p
;;;;;;  semantic-tag-static-p semantic-tag-leaf-p semantic-tag-abstract-p
;;;;;;  semantic-tag-protected-p semantic-tag-protection semantic-tag-calculate-parent)
;;;;;;  "semantic-tag-ls" "cedet/semantic/semantic-tag-ls.el" (19358
;;;;;;  14567))
;;; Generated autoloads from cedet/semantic/semantic-tag-ls.el

(autoload 'semantic-tag-calculate-parent "semantic-tag-ls" "\
Attempt to calculate the parent of TAG.
The default behavior (if not overriden with `tag-calculate-parent')
is to search a buffer found with TAG, and if externally defined,
search locally, then semanticdb for that tag (when enabled.)

\(fn TAG)" nil nil)

(autoload 'semantic-tag-protection "semantic-tag-ls" "\
Return protection information about TAG with optional PARENT.
This function returns on of the following symbols:
   nil        - No special protection.  Language dependent.
   'public    - Anyone can access this TAG.
   'private   - Only methods in the local scope can access TAG.
   'protected - Like private for outside scopes, like public for child
                classes.
Some languages may choose to provide additional return symbols specific
to themselves.  Use of this function should allow for this.

The default behavior (if not overridden with `tag-protection'
is to return a symbol based on type modifiers.

\(fn TAG &optional PARENT)" nil nil)

(autoload 'semantic-tag-protected-p "semantic-tag-ls" "\
Non-nil if TAG is is protected.
PROTECTION is a symbol which can be returned by the method
`semantic-tag-protection'.
PARENT is the parent data type which contains TAG.

For these PROTECTIONs, true is returned if TAG is:
@table @asis
@item nil
  Always true
@item  private
  True if nil.
@item protected
  True if private or nil.
@item public
  True if private, protected, or nil.
@end table

\(fn TAG PROTECTION &optional PARENT)" nil nil)

(autoload 'semantic-tag-abstract-p "semantic-tag-ls" "\
Return non nil if TAG is abstract.
Optional PARENT is the parent tag of TAG.
In UML, abstract methods and classes have special meaning and behavior
in how methods are overridden.  In UML, abstract methods are italicized.

The default behavior (if not overridden with `tag-abstract-p'
is to return true if `abstract' is in the type modifiers.

\(fn TAG &optional PARENT)" nil nil)

(autoload 'semantic-tag-leaf-p "semantic-tag-ls" "\
Return non nil if TAG is leaf.
Optional PARENT is the parent tag of TAG.
In UML, leaf methods and classes have special meaning and behavior.

The default behavior (if not overridden with `tag-leaf-p'
is to return true if `leaf' is in the type modifiers.

\(fn TAG &optional PARENT)" nil nil)

(autoload 'semantic-tag-static-p "semantic-tag-ls" "\
Return non nil if TAG is static.
Optional PARENT is the parent tag of TAG.
In UML, static methods and attributes mean that they are allocated
in the parent class, and are not instance specific.
UML notation specifies that STATIC entries are underlined.

\(fn TAG &optional PARENT)" nil nil)

(autoload 'semantic-tag-prototype-p "semantic-tag-ls" "\
Return non nil if TAG is a prototype.
For some laguages, such as C, a prototype is a declaration of
something without an implementation.

\(fn TAG)" nil nil)

(autoload 'semantic-tag-full-name "semantic-tag-ls" "\
Return the fully qualified name of TAG in the package hierarchy.
STREAM-OR-BUFFER can be anything convertable by `semantic-something-to-stream',
but must be a toplevel semantic tag stream that contains TAG.
A Package Hierarchy is defined in UML by the way classes and methods
are organized on disk.  Some language use this concept such that a
class can be accessed via it's fully qualified name, (such as Java.)
Other languages qualify names within a Namespace (such as C++) which
result in a different package like structure.  Languages which do not
override this function with `tag-full-name' will use
`semantic-tag-name'.  Override functions only need to handle
STREAM-OR-BUFFER with a tag stream value, or nil.

\(fn TAG &optional STREAM-OR-BUFFER)" nil nil)

;;;***

;;;### (autoloads (semantic-tag-write-list-slot-value semantic-tag-write-tag-list)
;;;;;;  "semantic-tag-write" "cedet/semantic/semantic-tag-write.el"
;;;;;;  (19358 14567))
;;; Generated autoloads from cedet/semantic/semantic-tag-write.el

(autoload 'semantic-tag-write-tag-list "semantic-tag-write" "\
Write the tag list TLIST to the current stream.
INDENT indicates the current indentation level.
If optional DONTADDNEWLINE is non-nil, then don't add a newline.

\(fn TLIST &optional INDENT DONTADDNEWLINE)" nil nil)

(autoload 'semantic-tag-write-list-slot-value "semantic-tag-write" "\
Write out the VALUE of a slot for EIEIO.
The VALUE is a list of tags.

\(fn VALUE)" nil nil)

;;;***

;;;### (autoloads (semantic-default-texi-setup) "semantic-texi" "cedet/semantic/semantic-texi.el"
;;;;;;  (19358 14567))
;;; Generated autoloads from cedet/semantic/semantic-texi.el

(autoload 'semantic-default-texi-setup "semantic-texi" "\
Set up a buffer for parsing of Texinfo files.

\(fn)" nil nil)

(add-hook 'texinfo-mode-hook 'semantic-default-texi-setup)

;;;***

;;;### (autoloads (semantic-utest-main) "semantic-utest" "cedet/semantic/semantic-utest.el"
;;;;;;  (19403 32036))
;;; Generated autoloads from cedet/semantic/semantic-utest.el

(autoload 'semantic-utest-main "semantic-utest" "\
Not documented

\(fn)" t nil)

;;;***

;;;### (autoloads (semantic-utest-c) "semantic-utest-c" "cedet/semantic/semantic-utest-c.el"
;;;;;;  (19328 24678))
;;; Generated autoloads from cedet/semantic/semantic-utest-c.el

(autoload 'semantic-utest-c "semantic-utest-c" "\
Run parsing test for C from the test directory.

\(fn)" t nil)

;;;***

;;;### (autoloads (semantic-highlight-func-mode global-semantic-highlight-func-mode
;;;;;;  global-semantic-highlight-func-mode semantic-stickyfunc-mode
;;;;;;  global-semantic-stickyfunc-mode global-semantic-stickyfunc-mode
;;;;;;  semantic-show-parser-state-mode global-semantic-show-parser-state-mode
;;;;;;  global-semantic-show-parser-state-mode semantic-show-unmatched-syntax-mode
;;;;;;  global-semantic-show-unmatched-syntax-mode global-semantic-show-unmatched-syntax-mode
;;;;;;  semantic-highlight-edits-mode global-semantic-highlight-edits-mode
;;;;;;  global-semantic-highlight-edits-mode) "semantic-util-modes"
;;;;;;  "cedet/semantic/semantic-util-modes.el" (19390 36854))
;;; Generated autoloads from cedet/semantic/semantic-util-modes.el

(autoload 'global-semantic-highlight-edits-mode "semantic-util-modes" "\
Toggle global use of option `semantic-highlight-edits-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

(defvar global-semantic-highlight-edits-mode nil "\
*If non-nil enable global use of variable `semantic-highlight-edits-mode'.
When this mode is enabled, changes made to a buffer are highlighted
until the buffer is reparsed.")

(custom-autoload 'global-semantic-highlight-edits-mode "semantic-util-modes" nil)

(autoload 'semantic-highlight-edits-mode "semantic-util-modes" "\
Minor mode for highlighting changes made in a buffer.
Changes are tracked by semantic so that the incremental parser can work
properly.
This mode will highlight those changes as they are made, and clear them
when the incremental parser accounts for those edits.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\(fn &optional ARG)" t nil)

(autoload 'global-semantic-show-unmatched-syntax-mode "semantic-util-modes" "\
Toggle global use of option `semantic-show-unmatched-syntax-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

(defvar global-semantic-show-unmatched-syntax-mode nil "\
If non-nil, enable global use of `semantic-show-unmatched-syntax-mode'.
When this mode is enabled, syntax in the current buffer which the
semantic parser cannot match is highlighted with a red underline.")

(custom-autoload 'global-semantic-show-unmatched-syntax-mode "semantic-util-modes" nil)

(autoload 'semantic-show-unmatched-syntax-mode "semantic-util-modes" "\
Minor mode to highlight unmatched lexical syntax tokens.
When a parser executes, some elements in the buffer may not match any
parser rules.  These text characters are considered unmatched syntax.
Often time, the display of unmatched syntax can expose coding
problems before the compiler is run.

With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\\{semantic-show-unmatched-syntax-mode-map}

\(fn &optional ARG)" t nil)

(defvar global-semantic-show-parser-state-mode nil "\
*If non-nil enable global use of `semantic-show-parser-state-mode'.
When enabled, the current parse state of the current buffer is displayed
in the mode line.  See `semantic-show-parser-state-marker' for details
on what is displayed.")

(custom-autoload 'global-semantic-show-parser-state-mode "semantic-util-modes" nil)

(autoload 'global-semantic-show-parser-state-mode "semantic-util-modes" "\
Toggle global use of option `semantic-show-parser-state-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

(autoload 'semantic-show-parser-state-mode "semantic-util-modes" "\
Minor mode for displaying parser cache state in the modeline.
The cache can be in one of three states.  They are
Up to date, Partial reparse needed, and Full reparse needed.
The state is indicated in the modeline with the following characters:
 `-'  ->  The cache is up to date.
 `!'  ->  The cache requires a full update.
 `~'  ->  The cache needs to be incrementally parsed.
 `%'  ->  The cache is not currently parseable.
 `@'  ->  Auto-parse in progress (not set here.)
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\(fn &optional ARG)" t nil)

(autoload 'global-semantic-stickyfunc-mode "semantic-util-modes" "\
Toggle global use of option `semantic-stickyfunc-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

(defvar global-semantic-stickyfunc-mode nil "\
*If non-nil, enable global use of `semantic-stickyfunc-mode'.
This minor mode only works for Emacs 21 or later.
When enabled, the header line is enabled, and the first line
of the current function or method is displayed in it.
This makes it appear that the first line of that tag is
`sticky' to the top of the window.")

(custom-autoload 'global-semantic-stickyfunc-mode "semantic-util-modes" nil)

(autoload 'semantic-stickyfunc-mode "semantic-util-modes" "\
Minor mode to show the title of a tag in the header line.
Enables/disables making the header line of functions sticky.
A function (or other tag class specified by
`semantic-stickyfunc-sticky-classes') has a header line, meaning the
first line which describes the rest of the construct.  This first
line is what is displayed in the header line.

With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\(fn &optional ARG)" t nil)

(autoload 'global-semantic-highlight-func-mode "semantic-util-modes" "\
Toggle global use of option `semantic-highlight-func-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

(defvar global-semantic-highlight-func-mode nil "\
If non-nil, enable global use of `semantic-highlight-func-mode'.
When enabled, the first line of the current tag is highlighted.")

(custom-autoload 'global-semantic-highlight-func-mode "semantic-util-modes" nil)

(autoload 'semantic-highlight-func-mode "semantic-util-modes" "\
Minor mode to highlight the first line of the current tag.
Enables/disables making current function first line light up.
A function (or other tag class specified by
`semantic-stickyfunc-sticky-classes') is highlighted, meaning the
first line which describes the rest of the construct.

See `semantic-stickyfunc-mode' for putting a function in the
header line.  This mode recycles the stickyfunc configuration
classes list.

With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (semanticdb-file-stream semanticdb-file-table-object
;;;;;;  semanticdb-current-database) "semanticdb" "cedet/semantic/semanticdb.el"
;;;;;;  (19422 9004))
;;; Generated autoloads from cedet/semantic/semanticdb.el

(autoload 'semanticdb-current-database "semanticdb" "\
Return the currently active database.

\(fn)" nil nil)

(autoload 'semanticdb-file-table-object "semanticdb" "\
Return a semanticdb table belonging to FILE, make it up to date.
If file has database tags available in the database, return it.
If file does not have tags available, and DONTLOAD is nil,
then load the tags for FILE, and create a new table object for it.
DONTLOAD does not affect the creation of new database objects.

\(fn FILE &optional DONTLOAD)" nil nil)

(autoload 'semanticdb-file-stream "semanticdb" "\
Return a list of tags belonging to FILE.
If file has database tags available in the database, return them.
If file does not have tags available, then load the file, and create them.

\(fn FILE)" nil nil)

;;;***

;;;### (autoloads (semanticdb-enable-cscope-databases) "semanticdb-cscope"
;;;;;;  "cedet/semantic/semanticdb-cscope.el" (19531 32699))
;;; Generated autoloads from cedet/semantic/semanticdb-cscope.el

(autoload 'semanticdb-enable-cscope-databases "semanticdb-cscope" "\
Enable the use of the CScope back end for all files in C/C++.
This will add an instance of a CScope database to each buffer in a
CScope supported hierarchy.

\(fn)" nil nil)

;;;***

;;;### (autoloads (semanticdb-database-sanity-check semanticdb-table-sanity-check
;;;;;;  semanticdb-table-oob-sanity-check semanticdb-adebug-project-database-list
;;;;;;  semanticdb-adebug-current-table semanticdb-adebug-current-database
;;;;;;  semanticdb-dump-all-table-summary) "semanticdb-debug" "cedet/semantic/semanticdb-debug.el"
;;;;;;  (19358 14567))
;;; Generated autoloads from cedet/semantic/semanticdb-debug.el

(autoload 'semanticdb-dump-all-table-summary "semanticdb-debug" "\
Dump a list of all databases in Emacs memory.

\(fn)" t nil)

(defalias 'semanticdb-adebug-database-list 'semanticdb-dump-all-table-summary)

(autoload 'semanticdb-adebug-current-database "semanticdb-debug" "\
Run ADEBUG on the current database.

\(fn)" t nil)

(autoload 'semanticdb-adebug-current-table "semanticdb-debug" "\
Run ADEBUG on the current database.

\(fn)" t nil)

(autoload 'semanticdb-adebug-project-database-list "semanticdb-debug" "\
Run ADEBUG on the current database.

\(fn)" t nil)

(autoload 'semanticdb-table-oob-sanity-check "semanticdb-debug" "\
Validate that CACHE tags do not have any overlays in them.

\(fn CACHE)" nil nil)

(autoload 'semanticdb-table-sanity-check "semanticdb-debug" "\
Validate the current semanticdb TABLE.

\(fn &optional TABLE)" t nil)

(autoload 'semanticdb-database-sanity-check "semanticdb-debug" "\
Validate the current semantic database.

\(fn)" t nil)

;;;***

;;;### (autoloads (semanticdb-ebrowse-load-helper semanticdb-load-ebrowse-caches
;;;;;;  semanticdb-create-ebrowse-database) "semanticdb-ebrowse"
;;;;;;  "cedet/semantic/semanticdb-ebrowse.el" (19373 12957))
;;; Generated autoloads from cedet/semantic/semanticdb-ebrowse.el

(autoload 'semanticdb-create-ebrowse-database "semanticdb-ebrowse" "\
Create an EBROWSE database for directory DIR.
The database file is stored in ~/.semanticdb, or whichever directory
is specified by `semanticdb-default-save-directory'.

\(fn DIR)" t nil)

(autoload 'semanticdb-load-ebrowse-caches "semanticdb-ebrowse" "\
Load all semanticdb controlled EBROWSE caches.

\(fn)" t nil)

(autoload 'semanticdb-ebrowse-load-helper "semanticdb-ebrowse" "\
Create the semanticdb database via ebrowse for directory.
If DIRECTORY is found to be defunct, it won't load the DB, and will
warn instead.

\(fn DIRECTORY)" nil nil)

;;;***

;;;### (autoloads (semanticdb-enable-exuberent-ctags) "semanticdb-ectag"
;;;;;;  "cedet/semantic/ctags/semanticdb-ectag.el" (18791 59641))
;;; Generated autoloads from cedet/semantic/ctags/semanticdb-ectag.el

(autoload 'semanticdb-enable-exuberent-ctags "semanticdb-ectag" "\
Enable the use of exuberent ctags for out-of-buffer parsing for MODE.
MODE is a `major-mode' symbol used.
Throws an error if `semantic-ectag-program' is not of the correct
version needed by Semantic ctags support.

\(fn MODE)" t nil)

;;;***

;;;### (autoloads (semanticdb-full-filename semanticdb-live-p semanticdb-file-loaded-p
;;;;;;  semanticdb-persistent-path semanticdb-default-save-directory
;;;;;;  semanticdb-default-file-name) "semanticdb-file" "cedet/semantic/semanticdb-file.el"
;;;;;;  (19403 27120))
;;; Generated autoloads from cedet/semantic/semanticdb-file.el

(defvar semanticdb-default-file-name "semantic.cache" "\
*File name of the semantic tag cache.")

(custom-autoload 'semanticdb-default-file-name "semanticdb-file" t)

(defvar semanticdb-default-save-directory (expand-file-name "~/.semanticdb") "\
*Directory name where semantic cache files are stored.
If this value is nil, files are saved in the current directory.  If the value
is a valid directory, then it overrides `semanticdb-default-file-name' and
stores caches in a coded file name in this directory.")

(custom-autoload 'semanticdb-default-save-directory "semanticdb-file" t)

(defvar semanticdb-persistent-path '(always) "\
*List of valid paths that semanticdb will cache tags to.
When `global-semanticdb-minor-mode' is active, tag lists will
be saved to disk when Emacs exits.  Not all directories will have
tags that should be saved.
The value should be a list of valid paths.  A path can be a string,
indicating a directory in which to save a variable.  An element in the
list can also be a symbol.  Valid symbols are `never', which will
disable any saving anywhere, `always', which enables saving
everywhere, or `project', which enables saving in any directory that
passes a list of predicates in `semanticdb-project-predicate-functions'.")

(custom-autoload 'semanticdb-persistent-path "semanticdb-file" t)

(eieio-defclass-autoload 'semanticdb-project-database-file '(semanticdb-project-database eieio-persistent) "semanticdb-file" "Database of file tables saved to disk.")

(autoload 'semanticdb-file-loaded-p "semanticdb-file" "\
Return the project belonging to FILENAME if it was already loaded.

\(fn FILENAME)" nil nil)

(autoload 'semanticdb-live-p "semanticdb-file" "\
Return non-nil if the file associated with OBJ is live.
Live databases are objects associated with existing directories.

\(fn (OBJ semanticdb-project-database))" nil nil)

(autoload 'semanticdb-full-filename "semanticdb-file" "\
Fetch the full filename that OBJ refers to.

\(fn (OBJ semanticdb-project-database-file))" nil nil)

;;;***

;;;### (autoloads (semanticdb-find-tags-subclasses-of-type semanticdb-find-tags-external-children-of-type
;;;;;;  semanticdb-brute-find-tags-by-class semanticdb-brute-deep-find-tags-for-completion
;;;;;;  semanticdb-brute-deep-find-tags-by-name semanticdb-deep-find-tags-for-completion
;;;;;;  semanticdb-deep-find-tags-by-name-regexp semanticdb-deep-find-tags-by-name
;;;;;;  semanticdb-find-tags-by-class semanticdb-find-tags-for-completion
;;;;;;  semanticdb-find-tags-by-name-regexp semanticdb-find-tags-by-name
;;;;;;  semanticdb-find-tags-collector semanticdb-find-result-mapc
;;;;;;  semanticdb-find-result-nth-in-buffer semanticdb-find-result-nth
;;;;;;  semanticdb-find-result-with-nil-p semanticdb-find-results-p
;;;;;;  semanticdb-fast-strip-find-results semanticdb-strip-find-results
;;;;;;  semanticdb-find-adebug-scanned-includes semanticdb-test-current-database-list
;;;;;;  semanticdb-find-test-translate-path semanticdb-find-table-for-include
;;;;;;  semanticdb-find-translate-path-default semanticdb-find-default-throttle)
;;;;;;  "semanticdb-find" "cedet/semantic/semanticdb-find.el" (19546
;;;;;;  10695))
;;; Generated autoloads from cedet/semantic/semanticdb-find.el

(defvar semanticdb-find-throttle-custom-list '(repeat (radio (const 'local) (const 'project) (const 'unloaded) (const 'system) (const 'recursive) (const 'omniscience))) "\
Customization values for semanticdb find throttle.
See `semanticdb-find-throttle' for details.")

(defvar semanticdb-find-default-throttle '(local project unloaded system recursive) "\
The default throttle for `semanticdb-find' routines.
The throttle controls how detailed the list of database
tables is for a symbol lookup.  The value is a list with
the following keys:
  `file'       - The file the search is being performed from.
                 This option is here for completeness only, and
                 is assumed to always be on.
  `local'      - Tables from the same local directory are included.
                 This includes files directly referenced by a file name
                 which might be in a different directory.
  `project'    - Tables from the same local project are included
                 If `project' is specified, then `local' is assumed.
  `unloaded'   - If a table is not in memory, load it.  If it is not cached
                 on disk either, get the source, parse it, and create
                 the table.
  `system'     - Tables from system databases.  These are specifically
                 tables from system header files, or language equivalent.
  `recursive'  - For include based searches, includes tables referenced
                 by included files.
  `omniscience' - Included system databases which are omniscience, or
                 somehow know everything.  Omniscience databases are found
                 in `semanticdb-project-system-databases'.
                 The Emacs Lisp system DB is an omniscience database.")

(custom-autoload 'semanticdb-find-default-throttle "semanticdb-find" t)

(autoload 'semanticdb-find-translate-path-default "semanticdb-find" "\
Translate PATH into a list of semantic tables.
If BRUTISH is non nil, return all tables associated with PATH.
Default action as described in `semanticdb-find-translate-path'.

\(fn PATH BRUTISH)" nil nil)

(autoload 'semanticdb-find-table-for-include "semanticdb-find" "\
For a single INCLUDETAG found in TABLE, find a `semanticdb-table' object
INCLUDETAG is a semantic TAG of class 'include.
TABLE is a semanticdb table that identifies where INCLUDETAG came from.
TABLE is optional if INCLUDETAG has an overlay of :filename attribute.

\(fn INCLUDETAG &optional TABLE)" nil nil)

(autoload 'semanticdb-find-test-translate-path "semanticdb-find" "\
Call and output results of `semanticdb-find-translate-path'.
With ARG non-nil, specify a BRUTISH translation.
See `semanticdb-find-default-throttle' and `semanticdb-project-roots'
for details on how this list is derived.

\(fn &optional ARG)" t nil)

(autoload 'semanticdb-test-current-database-list "semanticdb-find" "\
Call and output results of `semanticdb-current-database-list'.
Uses the `default-directory' to derive results.

\(fn)" t nil)

(autoload 'semanticdb-find-adebug-scanned-includes "semanticdb-find" "\
Translate the current path, then display the lost includes.
Examines the variable `semanticdb-find-lost-includes'.

\(fn)" t nil)

(autoload 'semanticdb-strip-find-results "semanticdb-find" "\
Strip a semanticdb search RESULTS to exclude objects.
This makes it appear more like the results of a `semantic-find-' call.
Optional FIND-FILE-MATCH loads all files associated with RESULTS
into buffers.  This has the side effect of enabling `semantic-tag-buffer' to
return a value.
If FIND-FILE-MATCH is 'name, then only the filename is stored
in each tag instead of loading each file into a buffer.
If the input RESULTS are not going to be used again, and if
FIND-FILE-MATCH is nil, you can use `semanticdb-fast-strip-find-results'
instead.

\(fn RESULTS &optional FIND-FILE-MATCH)" nil nil)

(autoload 'semanticdb-fast-strip-find-results "semanticdb-find" "\
Destructively strip a semanticdb search RESULTS to exclude objects.
This makes it appear more like the results of a `semantic-find-' call.
This is like `semanticdb-strip-find-results', except the input list RESULTS
will be changed.

\(fn RESULTS)" nil nil)

(autoload 'semanticdb-find-results-p "semanticdb-find" "\
Non-nil if RESULTP is in the form of a semanticdb search result.
This query only really tests the first entry in the list that is RESULTP,
but should be good enough for debugging assertions.

\(fn RESULTP)" nil nil)

(autoload 'semanticdb-find-result-with-nil-p "semanticdb-find" "\
Non-nil of RESULTP is in the form of a semanticdb search result.
The value nil is valid where a TABLE usually is, but only if the TAG
results include overlays.
This query only really tests the first entry in the list that is RESULTP,
but should be good enough for debugging assertions.

\(fn RESULTP)" nil nil)

(autoload 'semanticdb-find-result-nth "semanticdb-find" "\
In RESULT, return the Nth search result.
This is a 0 based search result, with the first match being element 0.

The returned value is a cons cell: (TAG . TABLE) where TAG
is the tag at the Nth position.  TABLE is the semanticdb table where
the TAG was found.  Sometimes TABLE can be nil.

\(fn RESULT N)" nil nil)

(autoload 'semanticdb-find-result-nth-in-buffer "semanticdb-find" "\
In RESULT, return the Nth search result.
Like `semanticdb-find-result-nth', except that only the TAG
is returned, and the buffer it is found it will be made current.
If the result tag has no position information, the originating buffer
is still made current.

\(fn RESULT N)" nil nil)

(autoload 'semanticdb-find-result-mapc "semanticdb-find" "\
Apply FCN to each element of find RESULT for side-effects only.
FCN takes two arguments.  The first is a TAG, and the
second is a DB from whence TAG originated.
Returns result.

\(fn FCN RESULT)" nil nil)

(autoload 'semanticdb-find-tags-collector "semanticdb-find" "\
Collect all tags returned by FUNCTION over PATH.
The FUNCTION must take two arguments.  The first is TABLE,
which is a semanticdb table containing tags.  The second argument
to FUNCTION is TAGS.  TAGS may be a list of tags.  If TAGS is non-nil,
then FUNCTION should search the TAG list, not through TABLE.

See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer.

Note: You should leave FIND-FILE-MATCH as nil.  It is far more
efficient to take the results from any search and use
`semanticdb-strip-find-results' instead.  This argument is here
for backward compatibility.

If optional argument BRUTISH is non-nil, then ignore include statements,
and search all tables in this project tree.

\(fn FUNCTION &optional PATH FIND-FILE-MATCH BRUTISH)" nil nil)

(autoload 'semanticdb-find-tags-by-name "semanticdb-find" "\
Search for all tags matching NAME on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer.

\(fn NAME &optional PATH FIND-FILE-MATCH)" nil nil)

(autoload 'semanticdb-find-tags-by-name-regexp "semanticdb-find" "\
Search for all tags matching REGEXP on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer.

\(fn REGEXP &optional PATH FIND-FILE-MATCH)" nil nil)

(autoload 'semanticdb-find-tags-for-completion "semanticdb-find" "\
Search for all tags matching PREFIX on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer.

\(fn PREFIX &optional PATH FIND-FILE-MATCH)" nil nil)

(autoload 'semanticdb-find-tags-by-class "semanticdb-find" "\
Search for all tags of CLASS on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer.

\(fn CLASS &optional PATH FIND-FILE-MATCH)" nil nil)

(autoload 'semanticdb-deep-find-tags-by-name "semanticdb-find" "\
Search for all tags matching NAME on PATH.
Search also in all components of top level tags founds.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer.

\(fn NAME &optional PATH FIND-FILE-MATCH)" nil nil)

(autoload 'semanticdb-deep-find-tags-by-name-regexp "semanticdb-find" "\
Search for all tags matching REGEXP on PATH.
Search also in all components of top level tags founds.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer.

\(fn REGEXP &optional PATH FIND-FILE-MATCH)" nil nil)

(autoload 'semanticdb-deep-find-tags-for-completion "semanticdb-find" "\
Search for all tags matching PREFIX on PATH.
Search also in all components of top level tags founds.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer.

\(fn PREFIX &optional PATH FIND-FILE-MATCH)" nil nil)

(autoload 'semanticdb-brute-deep-find-tags-by-name "semanticdb-find" "\
Search for all tags matching NAME on PATH.
See `semanticdb-find-translate-path' for details on PATH.
The argument BRUTISH will be set so that searching includes all tables
in the current project.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated wit that tag should be loaded into a buffer.

\(fn NAME &optional PATH FIND-FILE-MATCH)" nil nil)

(autoload 'semanticdb-brute-deep-find-tags-for-completion "semanticdb-find" "\
Search for all tags matching PREFIX on PATH.
See `semanticdb-find-translate-path' for details on PATH.
The argument BRUTISH will be set so that searching includes all tables
in the current project.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated wit that tag should be loaded into a buffer.

\(fn PREFIX &optional PATH FIND-FILE-MATCH)" nil nil)

(autoload 'semanticdb-brute-find-tags-by-class "semanticdb-find" "\
Search for all tags of CLASS on PATH.
See `semanticdb-find-translate-path' for details on PATH.
The argument BRUTISH will be set so that searching includes all tables
in the current project.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer.

\(fn CLASS &optional PATH FIND-FILE-MATCH)" nil nil)

(autoload 'semanticdb-find-tags-external-children-of-type "semanticdb-find" "\
Search for all tags defined outside of TYPE w/ TYPE as a parent.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer.

\(fn TYPE &optional PATH FIND-FILE-MATCH)" nil nil)

(autoload 'semanticdb-find-tags-subclasses-of-type "semanticdb-find" "\
Search for all tags of class type defined that subclass TYPE.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer.

\(fn TYPE &optional PATH FIND-FILE-MATCH)" nil nil)

;;;***

;;;### (autoloads (semanticdb-enable-gnu-global-databases) "semanticdb-global"
;;;;;;  "cedet/semantic/semanticdb-global.el" (19531 32448))
;;; Generated autoloads from cedet/semantic/semanticdb-global.el

(autoload 'semanticdb-enable-gnu-global-databases "semanticdb-global" "\
Enable the use of the GNU Global SemanticDB back end for all files of MODE.
This will add an instance of a GNU Global database to each buffer
in a GNU Global supported hierarchy.

\(fn MODE)" t nil)

;;;***

;;;### (autoloads (global-semanticdb-minor-mode semanticdb-minor-mode-p
;;;;;;  semanticdb-global-mode) "semanticdb-mode" "cedet/semantic/semanticdb-mode.el"
;;;;;;  (19137 25554))
;;; Generated autoloads from cedet/semantic/semanticdb-mode.el

(defvar semanticdb-current-database nil "\
For a given buffer, this is the currently active database.")

(defvar semanticdb-current-table nil "\
For a given buffer, this is the currently active database table.")

(defvar semanticdb-global-mode nil "\
*If non-nil enable the use of `semanticdb-minor-mode'.")

(custom-autoload 'semanticdb-global-mode "semanticdb-mode" nil)

(autoload 'semanticdb-minor-mode-p "semanticdb-mode" "\
Return non-nil if `semanticdb-minor-mode' is active.

\(fn)" nil nil)

(autoload 'global-semanticdb-minor-mode "semanticdb-mode" "\
Toggle the use of `semanticdb-minor-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (semanticdb-add-reference) "semanticdb-ref" "cedet/semantic/semanticdb-ref.el"
;;;;;;  (19269 17908))
;;; Generated autoloads from cedet/semantic/semanticdb-ref.el

(autoload 'semanticdb-add-reference "semanticdb-ref" "\
Add a reference for the database table DBT based on INCLUDE-TAG.
DBT is the database table that owns the INCLUDE-TAG.  The reference
will be added to the database that INCLUDE-TAG refers to.

\(fn (DBT semanticdb-abstract-table) INCLUDE-TAG)" nil nil)

;;;***

;;;### (autoloads (semanticdb-find-nonterminal-by-function semanticdb-find-nonterminal-by-extra-spec-value
;;;;;;  semanticdb-find-nonterminal-by-extra-spec semanticdb-find-nonterminal-by-property
;;;;;;  semanticdb-find-nonterminal-by-type semanticdb-find-nonterminal-by-name-regexp
;;;;;;  semanticdb-find-nonterminal-by-name semanticdb-find-nonterminal-by-token)
;;;;;;  "semanticdb-search" "cedet/semantic/semanticdb-search.el"
;;;;;;  (19099 11099))
;;; Generated autoloads from cedet/semantic/semanticdb-search.el

(autoload 'semanticdb-find-nonterminal-by-token "semanticdb-search" "\
OBSOLETE:
Find all occurances of nonterminals with token TOKEN in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES, DIFF-MODE, FIND-FILE-MATCH and IGNORE-SYSTEM.
Return a list ((DB-TABLE . TOKEN-LIST) ...).

\(fn TOKEN &optional DATABASES SEARCH-PARTS SEARCH-INCLUDES DIFF-MODE FIND-FILE-MATCH IGNORE-SYSTEM)" nil nil)

(autoload 'semanticdb-find-nonterminal-by-name "semanticdb-search" "\
OBSOLETE:
Find all occurances of nonterminals with name NAME in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES, DIFF-MODE, FIND-FILE-MATCH and IGNORE-SYSTEM.
Return a list ((DB-TABLE . TOKEN) ...).

\(fn NAME &optional DATABASES SEARCH-PARTS SEARCH-INCLUDES DIFF-MODE FIND-FILE-MATCH IGNORE-SYSTEM)" nil nil)

(autoload 'semanticdb-find-nonterminal-by-name-regexp "semanticdb-search" "\
OBSOLETE:
Find all occurances of nonterminals with name matching REGEX in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, FIND-FILE-MATCH and IGNORE-SYSTEM.
Return a list ((DB-TABLE . TOKEN-LIST) ...).

\(fn REGEX &optional DATABASES SEARCH-PARTS SEARCH-INCLUDES DIFF-MODE FIND-FILE-MATCH IGNORE-SYSTEM)" nil nil)

(autoload 'semanticdb-find-nonterminal-by-type "semanticdb-search" "\
OBSOLETE:
Find all nonterminals with a type of TYPE in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, FIND-FILE-MATCH and IGNORE-SYSTEM.
Return a list ((DB-TABLE . TOKEN-LIST) ...).

\(fn TYPE &optional DATABASES SEARCH-PARTS SEARCH-INCLUDES DIFF-MODE FIND-FILE-MATCH IGNORE-SYSTEM)" nil nil)

(autoload 'semanticdb-find-nonterminal-by-property "semanticdb-search" "\
OBSOLETE:
Find all nonterminals with a PROPERTY equal to VALUE in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, FIND-FILE-MATCH and IGNORE-SYSTEM.
Return a list ((DB-TABLE . TOKEN-LIST) ...).

\(fn PROPERTY VALUE &optional DATABASES SEARCH-PARTS SEARCH-INCLUDES DIFF-MODE FIND-FILE-MATCH IGNORE-SYSTEM)" nil nil)

(autoload 'semanticdb-find-nonterminal-by-extra-spec "semanticdb-search" "\
OBSOLETE:
Find all nonterminals with a SPEC in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, FIND-FILE-MATCH and IGNORE-SYSTEM.
Return a list ((DB-TABLE . TOKEN-LIST) ...).

\(fn SPEC &optional DATABASES SEARCH-PARTS SEARCH-INCLUDES DIFF-MODE FIND-FILE-MATCH IGNORE-SYSTEM)" nil nil)

(autoload 'semanticdb-find-nonterminal-by-extra-spec-value "semanticdb-search" "\
OBSOLETE:
Find all nonterminals with a SPEC equal to VALUE in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, FIND-FILE-MATCH and IGNORE-SYSTEM.
Return a list ((DB-TABLE . TOKEN-LIST) ...).

\(fn SPEC VALUE &optional DATABASES SEARCH-PARTS SEARCH-INCLUDES DIFF-MODE FIND-FILE-MATCH IGNORE-SYSTEM)" nil nil)

(autoload 'semanticdb-find-nonterminal-by-function "semanticdb-search" "\
OBSOLETE:
Find all occurances of nonterminals which match FUNCTION.
Search in all DATABASES.  If DATABASES is nil, search a range of
associated databases calculated `semanticdb-current-database-list' and
DATABASES is a list of variable `semanticdb-project-database' objects.
When SEARCH-PARTS is non-nil the search will include children of tags.
When SEARCH-INCLUDES is non-nil, the search will include dependency files.
When DIFF-MODE is non-nil, search databases which are of a different mode.
A Mode is the `major-mode' that file was in when it was last parsed.
When FIND-FILE-MATCH is non-nil, the make sure any found token's file is
in an Emacs buffer.
When IGNORE-SYSTEM is non-nil, system libraries are not searched.
Return a list ((DB-TABLE . TOKEN-OR-TOKEN-LIST) ...).

\(fn FUNCTION &optional DATABASES SEARCH-PARTS SEARCH-INCLUDES DIFF-MODE FIND-FILE-MATCH IGNORE-SYSTEM)" nil nil)

;;;***

;;;### (autoloads (semanticdb-db-typecache-dump semanticdb-typecache-dump
;;;;;;  semanticdb-typecache-refresh-for-buffer semanticdb-typecache-find
;;;;;;  semanticdb-typecache-merge-streams semanticdb-get-typecache
;;;;;;  semanticdb-get-typecache semanticdb-typecache-add-dependant
;;;;;;  semanticdb-typecache-notify-reset) "semanticdb-typecache"
;;;;;;  "cedet/semantic/semanticdb-typecache.el" (19403 1457))
;;; Generated autoloads from cedet/semantic/semanticdb-typecache.el

(eieio-defclass-autoload 'semanticdb-typecache 'nil "semanticdb-typecache" "Structure for maintaining a typecache.")

(autoload 'semanticdb-typecache-notify-reset "semanticdb-typecache" "\
Do a reset from a notify from a table we depend on.

\(fn (TC semanticdb-typecache))" nil nil)

(autoload 'semanticdb-typecache-add-dependant "semanticdb-typecache" "\
Add into the local typecache a dependant DEP.

\(fn DEP)" nil nil)

(autoload 'semanticdb-get-typecache "semanticdb-typecache" "\
Retrieve the typecache from the semanticdb TABLE.
If there is no table, create one, and fill it in.

\(fn (TABLE semanticdb-abstract-table))" nil nil)

(eieio-defclass-autoload 'semanticdb-database-typecache '(semanticdb-abstract-db-cache) "semanticdb-typecache" "Structure for maintaining a typecache.")

(autoload 'semanticdb-get-typecache "semanticdb-typecache" "\
Retrieve the typecache from the semantic database DB.
If there is no table, create one, and fill it in.

\(fn (DB semanticdb-project-database))" nil nil)

(autoload 'semanticdb-typecache-merge-streams "semanticdb-typecache" "\
Merge into CACHE1 and CACHE2 together.  The Caches will be merged in place.

\(fn CACHE1 CACHE2)" nil nil)

(autoload 'semanticdb-typecache-find "semanticdb-typecache" "\
Search the typecache for TYPE in PATH.
If type is a string, split the string, and search for the parts.
If type is a list, treat the type as a pre-split string.
PATH can be nil for the current buffer, or a semanticdb table.
FIND-FILE-MATCH is non-nil to force all found tags to be loaded into a buffer.

\(fn TYPE &optional PATH FIND-FILE-MATCH)" nil nil)

(autoload 'semanticdb-typecache-refresh-for-buffer "semanticdb-typecache" "\
Refresh the typecache for BUFFER.

\(fn BUFFER)" nil nil)

(autoload 'semanticdb-typecache-dump "semanticdb-typecache" "\
Dump the typecache for the current buffer.

\(fn)" t nil)

(autoload 'semanticdb-db-typecache-dump "semanticdb-typecache" "\
Dump the typecache for the current buffer's database.

\(fn)" t nil)

;;;***

;;;### (autoloads (senator-try-expand-semantic global-senator-minor-mode
;;;;;;  senator-minor-mode senator-word-search-backward senator-re-search-backward
;;;;;;  senator-search-backward senator-word-search-forward senator-re-search-forward
;;;;;;  senator-search-forward senator-completion-menu-popup senator-complete-symbol
;;;;;;  senator-jump-regexp senator-jump senator-previous-tag senator-next-tag
;;;;;;  senator-step-at-start-end-tag-classes senator-step-at-tag-classes
;;;;;;  global-senator-minor-mode) "senator" "cedet/semantic/senator.el"
;;;;;;  (19467 49880))
;;; Generated autoloads from cedet/semantic/senator.el

(defvar global-senator-minor-mode nil "\
*If non-nil enable global use of senator minor mode.")

(custom-autoload 'global-senator-minor-mode "senator" nil)

(defvar senator-step-at-tag-classes nil "\
*List of tag classes where to step.
A tag class is a symbol like 'variable, 'function, 'type, or other.
If nil navigation steps at any tag found.  This is a buffer local
variable.  It can be set in a mode hook to get a specific langage
navigation.")

(custom-autoload 'senator-step-at-tag-classes "senator" t)

(defvar senator-step-at-start-end-tag-classes '(function) "\
*List of tag classes where to step at start and end.
A tag class is a symbol like 'variable, 'function, 'type, or other.
If nil, navigation only step at beginning of tags.  If t, step at
start and end of any tag where it is allowed to step.  Also, stepping
at start and end of a tag prevent stepping inside its components.
This is a buffer local variable.  It can be set in a mode hook to get
a specific langage navigation.")

(custom-autoload 'senator-step-at-start-end-tag-classes "senator" t)

(autoload 'senator-next-tag "senator" "\
Navigate to the next Semantic tag.
Return the tag or nil if at end of buffer.

\(fn)" t nil)

(autoload 'senator-previous-tag "senator" "\
Navigate to the previous Semantic tag.
Return the tag or nil if at beginning of buffer.

\(fn)" t nil)

(autoload 'senator-jump "senator" "\
Jump to the semantic symbol SYM.

If optional IN-CONTEXT is non-nil jump in the local type's context
\(see function `senator-current-type-context').  If optional
NO-DEFAULT is non-nil do not provide a default value.

When called interactively you can combine the IN-CONTEXT and
NO-DEFAULT switches like this:

- \\[universal-argument]       IN-CONTEXT.
- \\[universal-argument] -     NO-DEFAULT.
- \\[universal-argument] \\[universal-argument]   IN-CONTEXT + NO-DEFAULT.

\(fn SYM &optional IN-CONTEXT NO-DEFAULT)" t nil)

(autoload 'senator-jump-regexp "senator" "\
Jump to the semantic symbol SYMREGEX.
SYMREGEX is treated as a regular expression.

If optional IN-CONTEXT is non-nil jump in the local type's context
\(see function `senator-current-type-context').  If optional
NO-DEFAULT is non-nil do not provide a default value and move to the
next match of SYMREGEX.  NOTE: Doesn't actually work yet.

When called interactively you can combine the IN-CONTEXT and
NO-DEFAULT switches like this:

- \\[universal-argument]       IN-CONTEXT.
- \\[universal-argument] -     NO-DEFAULT.
- \\[universal-argument] \\[universal-argument]   IN-CONTEXT + NO-DEFAULT.

\(fn SYMREGEX &optional IN-CONTEXT NO-DEFAULT)" t nil)

(autoload 'senator-complete-symbol "senator" "\
Complete the current symbol under point.
If optional argument CYCLE-ONCE is non-nil, only cycle through the list
of completions once, doing nothing where there are no more matches.

\(fn &optional CYCLE-ONCE)" t nil)

(autoload 'senator-completion-menu-popup "senator" "\
Popup a completion menu for the symbol at point.
The popup menu displays all of the possible completions for the symbol
it was invoked on.  To automatically split large menus this function
use `imenu--mouse-menu' to handle the popup menu.

\(fn)" t nil)

(autoload 'senator-search-forward "senator" "\
Search in tag names forward from point for STRING.
Set point to the end of the occurrence found, and return point.
See also the function `search-forward' for details on the BOUND,
NOERROR and COUNT arguments.

\(fn STRING &optional BOUND NOERROR COUNT)" t nil)

(autoload 'senator-re-search-forward "senator" "\
Search in tag names forward from point for regular expression REGEXP.
Set point to the end of the occurrence found, and return point.
See also the function `re-search-forward' for details on the BOUND,
NOERROR and COUNT arguments.

\(fn REGEXP &optional BOUND NOERROR COUNT)" t nil)

(autoload 'senator-word-search-forward "senator" "\
Search in tag names forward from point for WORD.
Set point to the end of the occurrence found, and return point.
See also the function `word-search-forward' for details on the BOUND,
NOERROR and COUNT arguments.

\(fn WORD &optional BOUND NOERROR COUNT)" t nil)

(autoload 'senator-search-backward "senator" "\
Search in tag names backward from point for STRING.
Set point to the beginning of the occurrence found, and return point.
See also the function `search-backward' for details on the BOUND,
NOERROR and COUNT arguments.

\(fn STRING &optional BOUND NOERROR COUNT)" t nil)

(autoload 'senator-re-search-backward "senator" "\
Search in tag names backward from point for regular expression REGEXP.
Set point to the beginning of the occurrence found, and return point.
See also the function `re-search-backward' for details on the BOUND,
NOERROR and COUNT arguments.

\(fn REGEXP &optional BOUND NOERROR COUNT)" t nil)

(autoload 'senator-word-search-backward "senator" "\
Search in tag names backward from point for WORD.
Set point to the beginning of the occurrence found, and return point.
See also the function `word-search-backward' for details on the BOUND,
NOERROR and COUNT arguments.

\(fn WORD &optional BOUND NOERROR COUNT)" t nil)

(autoload 'senator-minor-mode "senator" "\
Toggle senator minor mode.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\\{senator-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'global-senator-minor-mode "senator" "\
Toggle global use of senator minor mode.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

(autoload 'senator-try-expand-semantic "senator" "\
Attempt inline completion at the cursor.
Use Semantic, or the semantic database to look up possible
completions.  The argument OLD has to be nil the first call of this
function.  It returns t if a unique, possibly partial, completion is
found, nil otherwise.

\(fn OLD)" nil nil)

;;;***

;;;### (autoloads (speedbar-get-focus speedbar-frame-mode) "speedbar"
;;;;;;  "cedet/speedbar/speedbar.el" (18881 38525))
;;; Generated autoloads from cedet/speedbar/speedbar.el

(defalias 'speedbar 'speedbar-frame-mode)

(autoload 'speedbar-frame-mode "speedbar" "\
Enable or disable speedbar.  Positive ARG means turn on, negative turn off.
A nil ARG means toggle.  Once the speedbar frame is activated, a buffer in
`speedbar-mode' will be displayed.  Currently, only one speedbar is
supported at a time.
`speedbar-before-popup-hook' is called before popping up the speedbar frame.
`speedbar-before-delete-hook' is called before the frame is deleted.

\(fn &optional ARG)" t nil)

(autoload 'speedbar-get-focus "speedbar" "\
Change frame focus to or from the speedbar frame.
If the selected frame is not speedbar, then speedbar frame is
selected.  If the speedbar frame is active, then select the attached frame.

\(fn)" t nil)

;;;***

;;;### (autoloads (srecode-semantic-handle-:kill srecode-semantic-handle-:system
;;;;;;  srecode-semantic-handle-:file srecode-semantic-handle-:time
;;;;;;  srecode-semantic-handle-:user srecode-semantic-handle-:region
;;;;;;  srecode-semantic-handle-:indent srecode-semantic-handle-:blank)
;;;;;;  "srecode-args" "cedet/srecode/srecode-args.el" (19373 12959))
;;; Generated autoloads from cedet/srecode/srecode-args.el

(autoload 'srecode-semantic-handle-:blank "srecode-args" "\
Add macros into the dictionary DICT specifying blank line spacing.
The wrapgap means make sure the first and last lines of the macro
do not contain any text from preceding or following text.

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-handle-:indent "srecode-args" "\
Add macros into the dictionary DICT for indentation.

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-handle-:region "srecode-args" "\
Add macros into the dictionary DICT based on the current :region.

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-handle-:user "srecode-args" "\
Add macros into the dictionary DICT based on the current :user.

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-handle-:time "srecode-args" "\
Add macros into the dictionary DICT based on the current :time.

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-handle-:file "srecode-args" "\
Add macros into the dictionary DICT based on the current :file.

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-handle-:system "srecode-args" "\
Add macros into the dictionary DICT based on the current :system.

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-handle-:kill "srecode-args" "\
Add macros into the dictionary DICT based on the kill ring.

\(fn DICT)" nil nil)

;;;***

;;;### (autoloads (srecode-compile-templates srecode-compile-file)
;;;;;;  "srecode-compile" "cedet/srecode/srecode-compile.el" (19442
;;;;;;  38441))
;;; Generated autoloads from cedet/srecode/srecode-compile.el

(autoload 'srecode-compile-file "srecode-compile" "\
Compile the templates from the file FNAME.

\(fn FNAME)" nil nil)

(autoload 'srecode-compile-templates "srecode-compile" "\
Compile a semantic recode template file into a mode-local variable.

\(fn)" t nil)

;;;***

;;;### (autoloads (srecode-semantic-handle-:cpp) "srecode-cpp" "cedet/srecode/srecode-cpp.el"
;;;;;;  (19428 39642))
;;; Generated autoloads from cedet/srecode/srecode-cpp.el

(autoload 'srecode-semantic-handle-:cpp "srecode-cpp" "\
Add macros into the dictionary DICT based on the current c++ file.
Adds the following:
FILENAME_SYMBOL - filename converted into a C compat symbol.
HEADER - Shown section if in a header file.

\(fn DICT)" nil nil)

;;;***

;;;### (autoloads (srecode-calculate-context) "srecode-ctxt" "cedet/srecode/srecode-ctxt.el"
;;;;;;  (19010 47297))
;;; Generated autoloads from cedet/srecode/srecode-ctxt.el

(autoload 'srecode-calculate-context "srecode-ctxt" "\
Calculate the context at the current point.
The returned context is a list, with the top-most context first.
Each returned context is a string that that would show up in a `context'
statement in an `.srt' file.

Some useful context values used by the provided srecode templates are:
  \"file\" - Templates that for a file (such as an empty file.)
     \"empty\" - The file is empty
  \"declaration\" - Top-level declarations in a file.
     \"include\" - In or near include statements
     \"package\" - In or near provide statements
     \"function\" - In or near function statements
         \"NAME\" - Near functions within NAME namespace or class
     \"variable\" - In or near variable statements.
     \"type\"     - In or near type declarations.
     \"comment\"  - In a comment
  \"classdecl\" - Declarations within a class/struct/etc.
     \"variable\" - In or near class fields
     \"function\" - In or near methods/functions
        \"virtual\" - Nearby items are virtual
           \"pure\" - and those virtual items are pure virtual
     \"type\"     - In or near type declarations.
     \"comment\"  - In a comment in a block of code
     -- these items show up at the end of the context list. --
     \"public\", \"protected\", \"private\" -
                  In or near a section of public/pritected/private entries.
  \"code\" - In a block of code.
     \"string\" - In a string in a block of code
     \"comment\"  - In a comment in a block of code

    ... More later.

\(fn)" nil nil)

;;;***

;;;### (autoloads (srecode-dictionary-dump srecode-adebug-dictionary
;;;;;;  srecode-create-dictionary) "srecode-dictionary" "cedet/srecode/srecode-dictionary.el"
;;;;;;  (19436 42677))
;;; Generated autoloads from cedet/srecode/srecode-dictionary.el

(eieio-defclass-autoload 'srecode-dictionary 'nil "srecode-dictionary" "Dictionary of symbols and what they mean.\nDictionaries are used to look up named symbols from\ntemplates to decide what to do with those symbols.")

(eieio-defclass-autoload 'srecode-dictionary-compound-value 'nil "srecode-dictionary" "A compound dictionary value.\nValues stored in a dictionary must be a STRING,\na dictionary for showing sections, or an instance of a subclass\nof this class.\n\nCompound dictionary values derive from this class, and must\nprovide a sequence of method implementations to convert into\na string.")

(eieio-defclass-autoload 'srecode-dictionary-compound-variable '(srecode-dictionary-compound-value) "srecode-dictionary" "A compound dictionary value for template file variables.\nYou can declare a variable in a template like this:\n\nset NAME \"str\" macro \"OTHERNAME\"\n\nwith appending various parts together in a list.")

(autoload 'srecode-create-dictionary "srecode-dictionary" "\
Create a dictionary for BUFFER.
If BUFFER-OR-PARENT is not specified, assume a buffer, and
use the current buffer.
If BUFFER-OR-PARENT is another dictionary, then remember the
parent within the new dictionary, and assume that BUFFER
is the same as belongs to the parent dictionary.
The dictionary is initialized with variables setup for that
buffer's table.
If BUFFER-OR-PARENT is t, then this dictionary should not be
associated with a buffer or parent.

\(fn &optional BUFFER-OR-PARENT)" nil nil)

(eieio-defclass-autoload 'srecode-field-value '(srecode-dictionary-compound-value) "srecode-dictionary" "When inserting values with editable field mode, a dictionary value.\nCompound values allow a field to be stored in the dictionary for when\nit is referenced a second time.  This compound value can then be\ninserted with a new editable field.")

(autoload 'srecode-adebug-dictionary "srecode-dictionary" "\
Run data-debug on this mode's dictionary.

\(fn)" t nil)

(autoload 'srecode-dictionary-dump "srecode-dictionary" "\
Dump a typical fabricated dictionary.

\(fn)" t nil)

;;;***

;;;### (autoloads (srecode-document-function-comment-extract-test
;;;;;;  srecode-document-insert-group-comments srecode-document-insert-variable-one-line-comment
;;;;;;  srecode-document-insert-function-comment srecode-document-insert-comment)
;;;;;;  "srecode-document" "cedet/srecode/srecode-document.el" (19358
;;;;;;  14567))
;;; Generated autoloads from cedet/srecode/srecode-document.el

(eval-after-load "srecode-mode" '(progn (srecode-add-code-generator 'srecode-document-insert-comment "Comments" "C")))

(autoload 'srecode-document-insert-comment "srecode-document" "\
Insert some comments.
Whack any comments that may be in the way and replace them.
If the region is active, then insert group function comments.
If the cursor is in a comment, figure out what kind of comment it is
  and replace it.
If the cursor is in a function, insert a function comment.
If the cursor is on a one line prototype, then insert post-fcn comments.

\(fn)" t nil)

(autoload 'srecode-document-insert-function-comment "srecode-document" "\
Insert or replace a function comment.
FCN-IN is the Semantic tag of the function to add a comment too.
If FCN-IN is not provided, the current tag is used instead.
It is assumed that the comment occurs just in front of FCN-IN.

\(fn &optional FCN-IN)" t nil)

(autoload 'srecode-document-insert-variable-one-line-comment "srecode-document" "\
Insert or replace a variable comment.
VAR-IN is the Semantic tag of the function to add a comment too.
If VAR-IN is not provided, the current tag is used instead.
It is assumed that the comment occurs just after VAR-IN.

\(fn &optional VAR-IN)" t nil)

(autoload 'srecode-document-insert-group-comments "srecode-document" "\
Insert group comments around the active between BEG and END.
If the region includes only parts of some tags, expand out
to the beginning and end of the tags on the region.
If there is only one tag in the region, complain.

\(fn BEG END)" t nil)

(autoload 'srecode-document-function-comment-extract-test "srecode-document" "\
Test old comment extraction.
Dump out the extracted dictionary.

\(fn)" t nil)

;;;***

;;;### (autoloads (srecode-semantic-handle-:el-custom srecode-semantic-handle-:el)
;;;;;;  "srecode-el" "cedet/srecode/srecode-el.el" (18368 52100))
;;; Generated autoloads from cedet/srecode/srecode-el.el

(autoload 'srecode-semantic-handle-:el "srecode-el" "\
Add macros into the dictionary DICT based on the current Emacs Lisp file.
Adds the following:
  PRENAME - The common name prefix of this file.

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-handle-:el-custom "srecode-el" "\
Add macros into the dictionary DICT based on the current Emacs Lisp file.
Adds the following:
  GROUP - The 'defgroup' name we guess you want for variables.
  FACEGROUP - The `defgroup' name you might want for faces.

\(fn DICT)" nil nil)

;;;***

;;;### (autoloads (srecode-insert-prototype-expansion) "srecode-expandproto"
;;;;;;  "cedet/srecode/srecode-expandproto.el" (17917 27924))
;;; Generated autoloads from cedet/srecode/srecode-expandproto.el

(autoload 'srecode-insert-prototype-expansion "srecode-expandproto" "\
Insert get/set methods for the current class.

\(fn)" t nil)

;;;***

;;;### (autoloads (srecode-field-utest) "srecode-fields" "cedet/srecode/srecode-fields.el"
;;;;;;  (19329 54359))
;;; Generated autoloads from cedet/srecode/srecode-fields.el

(eieio-defclass-autoload 'srecode-field '(srecode-overlaid) "srecode-fields" "Representation of one field.")

(autoload 'srecode-field-utest "srecode-fields" "\
Test the srecode field manager.

\(fn)" t nil)

;;;***

;;;### (autoloads (srecode-comment-prefix) "srecode-filters" "cedet/srecode/srecode-filters.el"
;;;;;;  (18791 54835))
;;; Generated autoloads from cedet/srecode/srecode-filters.el

(autoload 'srecode-comment-prefix "srecode-filters" "\
Prefix each line of STR with the comment prefix characters.

\(fn STR)" nil nil)

;;;***

;;;### (autoloads (srecode-read-template-name srecode-template-get-table-for-binding
;;;;;;  srecode-template-get-table srecode-load-tables-for-mode srecode-table)
;;;;;;  "srecode-find" "cedet/srecode/srecode-find.el" (19321 64769))
;;; Generated autoloads from cedet/srecode/srecode-find.el

(autoload 'srecode-table "srecode-find" "\
Return the currently active Semantic Recoder table for this buffer.
Optional argument MODE specifies the mode table to use.

\(fn &optional MODE)" nil nil)

(autoload 'srecode-load-tables-for-mode "srecode-find" "\
Load all the template files for MMODE.
Templates are found in the SRecode Template Map.
See `srecode-get-maps' for more.
APPNAME is the name of an application.  In this case,
all template files for that application will be loaded.

\(fn MMODE &optional APPNAME)" nil nil)

(autoload 'srecode-template-get-table "srecode-find" "\
Find in the template in mode table TAB, the template with TEMPLATE-NAME.
Optional argument CONTEXT specifies a context a particular template
would belong to.
Optional argument APPLICATION restricts searches to only template tables
belonging to a specific application.  If APPLICATION is nil, then only
tables that do not belong to an application will be searched.

\(fn (TAB srecode-mode-table) TEMPLATE-NAME &optional CONTEXT APPLICATION)" nil nil)

(autoload 'srecode-template-get-table-for-binding "srecode-find" "\
Find in the template name in mode table TAB, the template with BINDING.
Optional argument CONTEXT specifies a context a particular template
would belong to.
Optional argument APPLICATION restricts searches to only template tables
belonging to a specific application.  If APPLICATION is nil, then only
tables that do not belong to an application will be searched.

\(fn (TAB srecode-mode-table) BINDING &optional CONTEXT APPLICATION)" nil nil)

(autoload 'srecode-read-template-name "srecode-find" "\
Completing read for Semantic Recoder template names.
PROMPT is used to query for the name of the template desired.
INITIAL is the initial string to use.
HIST is a history variable to use.
DEFAULT is what to use if the user presses RET.

\(fn PROMPT &optional INITIAL HIST DEFAULT)" nil nil)

;;;***

;;;### (autoloads (srecode-insert-getset) "srecode-getset" "cedet/srecode/srecode-getset.el"
;;;;;;  (19358 14567))
;;; Generated autoloads from cedet/srecode/srecode-getset.el

(eval-after-load "srecode-mode" '(progn (srecode-add-code-generator 'srecode-insert-getset "Get/Set" "G")))

(autoload 'srecode-insert-getset "srecode-getset" "\
Insert get/set methods for the current class.
CLASS-IN is the semantic tag of the class to update.
FIELD-IN is the semantic tag, or string name, of the field to add.
If you do not specify CLASS-IN or FIELD-IN then a class and field
will be derived.

\(fn &optional CLASS-IN FIELD-IN)" t nil)

;;;***

;;;### (autoloads (srecode-insert-fcn srecode-insert srecode-insert-again)
;;;;;;  "srecode-insert" "cedet/srecode/srecode-insert.el" (19467
;;;;;;  65051))
;;; Generated autoloads from cedet/srecode/srecode-insert.el

(autoload 'srecode-insert-again "srecode-insert" "\
Insert the previously inserted template (by name) again.

\(fn)" t nil)

(autoload 'srecode-insert "srecode-insert" "\
Insert the template TEMPLATE-NAME into the current buffer at point.
DICT-ENTRIES are additional dictionary values to add.

\(fn TEMPLATE-NAME &rest DICT-ENTRIES)" t nil)

(autoload 'srecode-insert-fcn "srecode-insert" "\
Insert TEMPLATE using DICTIONARY into STREAM.
Optional SKIPRESOLVER means to avoid refreshing the tag list,
or resolving any template arguments.  It is assumed the caller
has set everything up already.

\(fn TEMPLATE DICTIONARY &optional STREAM SKIPRESOLVER)" nil nil)

;;;***

;;;### (autoloads (srecode-semantic-handle-:java) "srecode-java"
;;;;;;  "cedet/srecode/srecode-java.el" (18786 50373))
;;; Generated autoloads from cedet/srecode/srecode-java.el

(autoload 'srecode-semantic-handle-:java "srecode-java" "\
Add macros into the dictionary DICT based on the current java file.
Adds the following:
FILENAME_AS_PACKAGE - file/dir converted into a java package name.
FILENAME_AS_CLASS - file converted to a Java class name.

\(fn DICT)" nil nil)

;;;***

;;;### (autoloads (srecode-adebug-maps srecode-get-maps) "srecode-map"
;;;;;;  "cedet/srecode/srecode-map.el" (19474 55487))
;;; Generated autoloads from cedet/srecode/srecode-map.el

(autoload 'srecode-get-maps "srecode-map" "\
Get a list of maps relevant to the current buffer.
Optional argument RESET forces a reset of the current map.

\(fn &optional RESET)" t nil)

(autoload 'srecode-adebug-maps "srecode-map" "\
Run ADEBUG on the output of `srecode-get-maps'.

\(fn)" t nil)

;;;***

;;;### (autoloads (global-srecode-minor-mode srecode-minor-mode)
;;;;;;  "srecode-mode" "cedet/srecode/srecode-mode.el" (19474 55534))
;;; Generated autoloads from cedet/srecode/srecode-mode.el

(autoload 'srecode-minor-mode "srecode-mode" "\
Toggle srecode minor mode.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\\{srecode-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'global-srecode-minor-mode "srecode-mode" "\
Toggle global use of srecode minor mode.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (srecode-semantic-insert-tag srecode-semantic-apply-tag-to-dict-default
;;;;;;  srecode-semantic-apply-tag-to-dict srecode-semantic-handle-:tagtype
;;;;;;  srecode-semantic-handle-:tag) "srecode-semantic" "cedet/srecode/srecode-semantic.el"
;;;;;;  (19445 56588))
;;; Generated autoloads from cedet/srecode/srecode-semantic.el

(autoload 'srecode-semantic-handle-:tag "srecode-semantic" "\
Add macros into the dictionary DICT based on the current :tag.

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-handle-:tagtype "srecode-semantic" "\
Add macros into the dictionary DICT based on a tag of class type at point.
Assumes the cursor is in a tag of class type.  If not, throw an error.

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-apply-tag-to-dict "srecode-semantic" "\
Insert features of TAGOBJ into the dictionary DICT.
TAGOBJ is an object of class `srecode-semantic-tag'.  This class
is a compound inserter value.
DICT is a dictionary object.
At a minimum, this function will create dictionary macro for NAME.
It is also likely to create macros for TYPE (data type), function arguments,
variable default values, and other things.

\(fn TAGOBJ DICT)" nil nil)

(autoload 'srecode-semantic-apply-tag-to-dict-default "srecode-semantic" "\
Insert features of TAGOBJ into dictionary DICT.

\(fn TAGOBJ DICT)" nil nil)

(autoload 'srecode-semantic-insert-tag "srecode-semantic" "\
Insert TAG into a buffer using srecode templates at point.

Optional STYLE-OPTION is a list of minor configuration of styles,
such as the symbol 'prototype for prototype functions, or
'system for system includes, and 'doxygen, for a doxygen style
comment.

Optional third argument POINT-INSERT-FCN is a hook that is run after
TAG is inserted that allows an opportunity to fill in the body of
some thing.  This hook function is called with one argument, the TAG
being inserted.

The rest of the arguments are DICT-ENTRIES.  DICT-ENTRIES
is of the form ( NAME1 VALUE1 NAME2 VALUE2 ... NAMEn VALUEn).

The exact template used is based on the current context.
The template used is found within the toplevel context as calculated
by `srecode-calculate-context', such as `declaration', `classdecl',
or `code'.

For various conditions, this function looks for a template with
the name CLASS-tag, where CLASS is the tag class.  If it cannot
find that, it will look for that template in the `declaration'
context (if the current context was not `declaration').

If PROTOTYPE is specified, it will first look for templates with
the name CLASS-tag-prototype, or CLASS-prototype as above.

See `srecode-semantic-apply-tag-to-dict' for details on what is in
the dictionary when the templates are called.

This function returns to location in the buffer where the
inserted tag ENDS, and will leave point inside the inserted
text based on any occurrence of a point-inserter.  Templates such
as `function' will leave point where code might be inserted.

\(fn TAG &optional STYLE-OPTION POINT-INSERT-FCN &rest DICT-ENTRIES)" nil nil)

;;;***

;;;### (autoloads (srecode-semantic-handle-:srt srecode-read-major-mode-name
;;;;;;  srecode-read-variable-name) "srecode-srt" "cedet/srecode/srecode-srt.el"
;;;;;;  (19358 14567))
;;; Generated autoloads from cedet/srecode/srecode-srt.el

(autoload 'srecode-read-variable-name "srecode-srt" "\
Read in the name of a declared variable in the current SRT file.
PROMPT is the prompt to use.
INITIAL is the initial string.
HIST is the history value, otherwise `srecode-read-variable-name-history'
     is used.
DEFAULT is the default if RET is hit.

\(fn PROMPT &optional INITIAL HIST DEFAULT)" nil nil)

(autoload 'srecode-read-major-mode-name "srecode-srt" "\
Read in the name of a desired `major-mode'.
PROMPT is the prompt to use.
INITIAL is the initial string.
HIST is the history value, otherwise `srecode-read-variable-name-history'
     is used.
DEFAULT is the default if RET is hit.

\(fn PROMPT &optional INITIAL HIST DEFAULT)" nil nil)

(autoload 'srecode-semantic-handle-:srt "srecode-srt" "\
Add macros into the dictionary DICT based on the current SRT file.
Adds the following:
ESCAPE_START - This files value of escape_start
ESCAPE_END - This files value of escape_end
MODE - The mode of this buffer.  If not declared yet, guess.

\(fn DICT)" nil nil)

;;;***

;;;### (autoloads (srecode-dump-templates srecode-mode-table-new
;;;;;;  srecode-get-mode-table) "srecode-table" "cedet/srecode/srecode-table.el"
;;;;;;  (19358 14567))
;;; Generated autoloads from cedet/srecode/srecode-table.el

(autoload 'srecode-get-mode-table "srecode-table" "\
Get the SRecoder mode table for the major mode MODE.
Optional argument SOFT indicates to not make a new one if a table
was not found.

\(fn MODE)" nil nil)

(autoload 'srecode-mode-table-new "srecode-table" "\
Create a new template table for MODE in FILE.
INIT are the initialization parameters for the new template table.

\(fn MODE FILE &rest INIT)" nil nil)

(autoload 'srecode-dump-templates "srecode-table" "\
Dump a list of the current templates for MODE.

\(fn MODE)" t nil)

;;;***

;;;### (autoloads (srecode-template-setup-parser) "srecode-template"
;;;;;;  "cedet/srecode/srecode-template.el" (18335 12812))
;;; Generated autoloads from cedet/srecode/srecode-template.el

(autoload 'srecode-template-setup-parser "srecode-template" "\
Setup buffer for parse.

\(fn)" nil nil)

(add-hook 'srecode-template-mode-hook 'srecode-template-setup-parser)

;;;***

;;;### (autoloads (srecode-template-mode) "srecode-template-mode"
;;;;;;  "cedet/srecode/srecode-template-mode.el" (19442 39427))
;;; Generated autoloads from cedet/srecode/srecode-template-mode.el

(autoload 'srecode-template-mode "srecode-template-mode" "\
Major-mode for writing SRecode macros.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.srt$" . srecode-template-mode))

;;;***

;;;### (autoloads (srecode-utest-template-output) "srecode-test"
;;;;;;  "cedet/srecode/srecode-test.el" (19443 8325))
;;; Generated autoloads from cedet/srecode/srecode-test.el

(autoload 'srecode-utest-template-output "srecode-test" "\
Test various template insertion options.

\(fn)" t nil)

;;;***

;;;### (autoloads (srecode-utest-getset-output) "srecode-test-getset"
;;;;;;  "cedet/srecode/srecode-test-getset.el" (18810 36153))
;;; Generated autoloads from cedet/srecode/srecode-test-getset.el

(autoload 'srecode-utest-getset-output "srecode-test-getset" "\
Test various template insertion options.

\(fn)" t nil)

;;;***

;;;### (autoloads (srecode-texi-insert-tag-as-doc srecode-semantic-handle-:texitag
;;;;;;  srecode-semantic-handle-:texi srecode-texi-add-menu) "srecode-texi"
;;;;;;  "cedet/srecode/srecode-texi.el" (19329 60994))
;;; Generated autoloads from cedet/srecode/srecode-texi.el

(autoload 'srecode-texi-add-menu "srecode-texi" "\
Add an item into the current menu.  Add @node statements as well.
Argument NEWNODE is the name of the new node.

\(fn NEWNODE)" t nil)

(autoload 'srecode-semantic-handle-:texi "srecode-texi" "\
Add macros into the dictionary DICT based on the current texinfo file.
Adds the following:
  LEVEL - chapter, section, subsection, etc
  NEXTLEVEL - One below level

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-handle-:texitag "srecode-texi" "\
Add macros into the dictionary DICT based on the current :tag file.
Adds the following:
  TAGDOC - Texinfo formatted doc string for :tag.

\(fn DICT)" nil nil)

(define-mode-local-override semantic-insert-foreign-tag texinfo-mode (foreign-tag) "Insert FOREIGN-TAG from a foreign buffer in TAGFILE.\nAssume TAGFILE is a source buffer, and create a documentation\nthingy from it using the `document' tool." (srecode-texi-insert-tag-as-doc foreign-tag))

(autoload 'srecode-texi-insert-tag-as-doc "srecode-texi" "\
Insert TAG into the current buffer with SRecode.

\(fn TAG)" nil nil)

;;;***

;;;### (autoloads (display-in-minibuffer insert-in-minibuffer concat-w-faces
;;;;;;  erase-inactive-minibuffer erase-nonempty-inactive-minibuffer
;;;;;;  display-lines-containing non-empty-name-p empty-name-p) "strings"
;;;;;;  "strings.el" (19826 15213))
;;; Generated autoloads from strings.el

(autoload 'empty-name-p "strings" "\
Nil if NAME is nil or \"\", else t.

\(fn NAME)" nil (quote macro))

(autoload 'non-empty-name-p "strings" "\
NAME if non-nil and not \"\", else nil.

\(fn NAME)" nil (quote macro))

(autoload 'display-lines-containing "strings" "\
Display in BUFFER the lines of `current-buffer' containing STRING.
See also command `occur' which does this and much more.  As this does
less, it can be useful if you intend to manipulate the contents of
BUFFER, not just use it to find things in the `current-buffer'.

BUFFER is a buffer or its name (a string).  STRING is a string.
Optional third argument FLUSH-P, if non-nil, means to display lines of
`current-buffer' that do *not* contain STRING.
Interactively:
  BUFFER defaults to buffer \"*Lines Containing*\".
  STRING is read in the minibuffer, and defaults to `current-line-string'.
  FLUSH-P is the prefix argument.

\(fn BUFFER STRING &optional FLUSH-P)" t nil)

(autoload 'erase-nonempty-inactive-minibuffer "strings" "\
Erase the minibuffer, if inactive and not `minibuffer-empty-p'.
To do this at each user input event:
   (add-hook 'pre-command-hook 'erase-nonempty-inactive-minibuffer).

Note that `minibuffer-empty-p' is not infallible.  To make sure the
minibuffer is emptied, you can use the surer, though slower, function
`erase-inactive-minibuffer'.

\(fn)" t nil)

(autoload 'erase-inactive-minibuffer "strings" "\
Erase the minibuffer (remove its contents), provided it is inactive.
To ensure that the minibuffer is erased at each user input, do this:
   (add-hook 'pre-command-hook 'erase-inactive-minibuffer).
This is generally costly, however.  For a faster, though less sure,
alternative, see `erase-nonempty-inactive-minibuffer'.

\(fn)" t nil)

(autoload 'concat-w-faces "strings" "\
Return the string that is the concatenation of all ARGUMENTS.
Text (face) properties of any string arguments are preserved.

This is obsolete.  Use `concat' with `put-text-property' or
`propertize' now.

Items in arg list may be strings or numbers (see `insert-string'), or
nil (ignored).  As a special case, they may also be lists of the form
\(FACE OBJECT), where OBJECT is an object to be converted to a string
via (format \"%s\"), and FACE is the face in which to display the
resulting string.  If OBJECT is a string, any text properties
belonging to it are ignored.

NOTE: For versions of Emacs that do not have faces, a list of
      (FACE OBJECT) is simply treated as the string resulting from
      (format \"%s\" OBJECT).

\(fn &rest ARGUMENTS)" t nil)

(autoload 'insert-in-minibuffer "strings" "\
Insert ARGUMENTS in minibuffer, indefinitely, preserving faces.
The minibuffer is not erased before display.  If you want to ensure
that the minibuffer is erased at each user input event, then do this:
    (add-hook 'pre-command-hook 'erase-inactive-minibuffer)
or  (add-hook 'pre-command-hook 'erase-nonempty-inactive-minibuffer)

Text (face) properties of string arguments are preserved.

Items in arg list may be strings or numbers (see `insert-string'), or
nil (ignored).  As a special case, they may also be lists of the form
\(FACE OBJECT), where OBJECT is an object to be converted to a string
via (format \"%s\"), and FACE is the face in which to display the
resulting string.  If OBJECT is a string, any text properties
belonging to it are ignored.

NOTE: For versions of Emacs that do not have faces, a list of
      (FACE OBJECT) is simply treated as the string resulting from
      (format \"%s\" OBJECT).

\(fn &rest ARGUMENTS)" t nil)

(autoload 'display-in-minibuffer "strings" "\
Display ARGUMENTS in minibuffer, preserving their face properties.
This function essentially allows you to display messages with faces.

First arg OPTION determines the display behavior, as follows:

 OPTION values `event', `new', and a `natnump' erase the minibuffer
 before displaying.  Other values do not.  They are intended for later
 use to add to the contents of the minibuffer.

 OPTION values `event', `more-event' and an `integerp' are guaranteed
 to erase the minibuffer at some time after displaying.  Other values
 do not erase it afterward.  They allow you to later add more to the
 current contents of the minibuffer.  Remember that they leave the
 minibuffer with text in it.  They should therefore at some point be
 followed by something that erases the contents, such as
 `erase-inactive-minibuffer'.

 OPTION values `event' and a `natnump' are common, one-shot affairs.
 The other values are only used when you need to combine several
 submessages via separate calls.

 OPTION values `event' and `more-event' block Emacs execution until
 the next user event.  This means, among other things, that such a
 call should always be the last of a sequence of calls to
 `display-in-minibuffer'.

 Here are all the OPTION values:

 If a number: ARGS are displayed for that many seconds (`sit-for'),
      then the minibuffer is erased.  The minibuffer is also
      erased before the display of ARGS, iff the number is >= 0.
 If `event': ARGS are displayed until the next user event, after
      erasing the minibuffer.  (If ARGS = nil, this just affects
      the duration of the current minibuffer contents.)
 If `more-event': ARGS displayed until next user event, without
      first erasing minibuffer.  (If ARGS = nil, this just affects
      the duration of the current minibuffer contents.)
 If `new': ARGS displayed indefinitely, after erasing minibuffer.
      (If ARGS = nil, then this just erases the minibuffer.)
 Else (e.g. `more'): ARGS displayed indefinitely, without first
      erasing minibuffer.  (If ARGS = nil, then this is a no-op.)

If you cannot (or do not want to) explicitly program the ultimate
erasure of the minibuffer, and yet you do not want to block program
execution by waiting for a time elapse or a user input, you may still
ensure that the minibuffer is always erased at the next user input,
by doing either of these (the first is surer, but slower):
    (add-hook 'pre-command-hook 'erase-inactive-minibuffer)
or  (add-hook 'pre-command-hook 'erase-nonempty-inactive-minibuffer)

This can be a good thing to do (but it can also slow things down
considerably).  You may then freely use OPTION values other than
numbers, `event' and `more-event' (e.g.  `new' and `more'), without
fear of indefinite display.  However, user input between `new' and
`more' will then interfere with incremental display.  If you do arm
`pre-command-hook' this way, you can always inhibit erasure
temporarily by rebinding `pre-command-hook' to nil.


The remaining arguments, besides OPTION, may be strings or numbers
\(see `insert-string'), or nil (ignored).

As a special case, they (items in the ARGS list) may also be lists of
the form (FACE STRING), where STRING is a string and FACE is the face
in which it is to be displayed.  In this case, any text properties
belonging to STRING itself are ignored.


EXAMPLE (one-shot, without `sit-for', erasure upon user event):
  (display-in-minibuffer 'event \"a\" \"b\") ; Erase, then display until event.
  ...                                        ;  --> ab

EXAMPLE (multiple calls, without `sit-for', erasure upon user event):
  (display-in-minibuffer 'new \"a\" \"b\")  ; Erase, then display.
  ...                                       ;  --> ab
  (display-in-minibuffer 'more \"cd\")      ; Display (no erase).
  ...                                       ;  --> abcd
  (display-in-minibuffer 'more-event \"ef\"); Display until user event.
  ...                                       ;  --> abcdef

EXAMPLE (without `sit-for', explicit erasure later):

  (display-in-minibuffer 'new \"ab\")     ; Erase, then display.
  ...                                     ;  --> ab
  (display-in-minibuffer 'more \"cd\")    ; Display (no erase).
  ...                                     ;  --> abcd
  (display-in-minibuffer 'new)            ; Erase---same as
                                          ; (erase-inactive-minibuffer).
  ...                                     ;  -->

EXAMPLE (with positive `sit-for'):
  (display-in-minibuffer 3 \"abc\" \"def\") ; Erase, display 3 sec, erase.

EXAMPLE (with negative `sit-for'):
  (display-in-minibuffer new \"abc\" \"def\") ; Erase, then display.
  ...
  (display-in-minibuffer -3 \"gh\")         ; Display (\"abcdefgh\") 3 sec.


NOTE:
 This function is not very useful interactively, especially as regards
 different values of OPTION.  Interactive calls in fact always erase
 the minibuffer first.
 Regardless of this, if interactive, then OPTION is the numeric value
 of the prefix arg, if any.  If there is no prefix arg, behavior is as
 if OPTION were `event': display contents until the next user event.

\(fn OPTION &rest ARGUMENTS)" t nil)

;;;***

;;;### (autoloads (tabbar-local-mode tabbar-mode tabbar-forward-tab
;;;;;;  tabbar-backward-tab tabbar-forward-group tabbar-backward-group
;;;;;;  tabbar-forward tabbar-backward) "tabbar" "tabbar.el" (16094
;;;;;;  64565))
;;; Generated autoloads from tabbar.el

(autoload 'tabbar-backward "tabbar" "\
Select the previous available tab.
Depend on the setting of the option `tabbar-cycling-scope'.

\(fn)" t nil)

(autoload 'tabbar-forward "tabbar" "\
Select the next available tab.
Depend on the setting of the option `tabbar-cycling-scope'.

\(fn)" t nil)

(autoload 'tabbar-backward-group "tabbar" "\
Go to selected tab in the previous available group.

\(fn)" t nil)

(autoload 'tabbar-forward-group "tabbar" "\
Go to selected tab in the next available group.

\(fn)" t nil)

(autoload 'tabbar-backward-tab "tabbar" "\
Select the previous visible tab.

\(fn)" t nil)

(autoload 'tabbar-forward-tab "tabbar" "\
Select the next visible tab.

\(fn)" t nil)

(defvar tabbar-mode nil "\
Non-nil if Tabbar mode is enabled.
See the command `tabbar-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `tabbar-mode'.")

(custom-autoload 'tabbar-mode "tabbar" nil)

(autoload 'tabbar-mode "tabbar" "\
Toggle display of a tab bar in the header line.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.

\(fn &optional ARG)" t nil)

(autoload 'tabbar-local-mode "tabbar" "\
Toggle local display of the tab bar.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.
When on and tab bar global mode is on, if a buffer local value of
`header-line-format' exists, it is saved, then the local header line
is killed to show the tab bar.  When off, the saved local value of the
header line is restored, hiding the tab bar.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (find-fn-or-var-nearest-point near-point-y-distance
;;;;;;  near-point-x-distance) "thingatpt+" "thingatpt+.el" (19826
;;;;;;  14807))
;;; Generated autoloads from thingatpt+.el

(defvar near-point-x-distance 50 "\
*Maximum number of characters from point to search, left and right.
Used by functions that provide default text for minibuffer input.
Some functions might ignore or override this setting temporarily.")

(custom-autoload 'near-point-x-distance "thingatpt+" t)

(defvar near-point-y-distance 5 "\
*Maximum number of lines from point to search, up and down.
To constrain search to the same line as point, set this to zero.
Used by functions that provide default text for minibuffer input.
Some functions might ignore or override this setting temporarily.")

(custom-autoload 'near-point-y-distance "thingatpt+" t)

(autoload 'find-fn-or-var-nearest-point "thingatpt+" "\
Go to the definition of the function or variable nearest the cursor.
With a prefix arg, or if no function or variable is near the cursor,
prompt for the function or variable to find, instead.

\(fn &optional CONFIRMP)" t nil)

;;;***

;;;### (autoloads (tf-run-with-idle-timer tf-time-difference) "timerfunctions"
;;;;;;  "predictive/timerfunctions.el" (19433 45500))
;;; Generated autoloads from predictive/timerfunctions.el

(autoload 'tf-time-difference "timerfunctions" "\
Gives the time in seconds elaspsed from TIMESUB to TIMEPLUS.
Almost like (- TIMEPLUS TIMESUB ).

\(fn TIMEPLUS TIMESUB)" nil nil)

(autoload 'tf-run-with-idle-timer "timerfunctions" "\
Args are SECS, REPEAT, REDOSECS, REDOREPEAT, INCLUDERUNTIME,
FUNCTION and &rest ARGS.
Similar to run-with-idle-timer, except that provides more options.
Suppose you want emacs to run an action every REDOSECS for as long as
emacs remains idle.  Think you can do it with the emacs' 
run-with-idle-timer? Think again.. :)   That function will perform the
action exactly once every time emacs goes idle.  This funciton, 
tf-run-with-idle-timer *will* allow you to keep performing an action
as long as emacs remains idle.

SECS is the number of seconds to wait once emacs has first gone
idle. It can really be any expression whose at runtime yields a
number..  Note that the way run-with-idle-timer is defined, SECS will
unfortunately be evalled immediately after you call this function, but
redosecs will be *every* time emacs *remains* idle..yay..


If REDOREPEAT is non-nil, the action is repeated as long emacs remains
idle.  REDOSECS is the number of additional seconds (after the action
has been done) to wait if emacs remains idle before performing the
action again.  Again, redosecs does not have to be a number, it can be
any expression whose eval yields to a number...

If INCLUDERUNTIME is non-nil, REDOSECS is the number of
additional seconds to wait after the action has been invoked (not
finished).

If REPEAT is nonnil, the entire cycle is repeated every time emacs
next goes idle.. (as in the default run-with-idle-timer.

\(fn SECS REPEAT REDOSECS REDOREPEAT INCLUDERUNTIME FUNCTION &rest ARGS)" nil nil)

;;;***

;;;### (autoloads (tq-create) "tq" "emms/lisp/tq.el" (19793 16447))
;;; Generated autoloads from emms/lisp/tq.el

(autoload 'tq-create "tq" "\
Create and return a transaction queue communicating with PROCESS.
PROCESS should be a subprocess capable of sending and receiving
streams of bytes.  It may be a local process, or it may be connected
to a tcp server on another machine.

\(fn PROCESS)" nil nil)

;;;***

;;;### (autoloads (directory-tree-thing eieio-class-tree tree-test-it-all)
;;;;;;  "tree" "cedet/eieio/tree.el" (17213 40344))
;;; Generated autoloads from cedet/eieio/tree.el

(autoload 'tree-test-it-all "tree" "\
Try using various features of tree mode in a demo of it's display.

\(fn)" t nil)

(autoload 'eieio-class-tree "tree" "\
Displays a class tree using the TREE package in another buffer.
Optional argument ROOT-CLASS is the starting point.

\(fn &optional ROOT-CLASS)" t nil)

(autoload 'directory-tree-thing "tree" "\
Start at the current directory, and build a giant tree of files.
Argument PPATH is the path to the directory we are going to analyze.

\(fn PPATH)" t nil)

;;;***

;;;### (autoloads (update-autoloads-for-file-in-package-area update-autoloads-in-package-area)
;;;;;;  "update-auto-loads" "update-auto-loads.el" (19826 11553))
;;; Generated autoloads from update-auto-loads.el

(let ((new-path (expand-file-name (file-name-directory load-file-name)))) (unless (member new-path (mapcar '(lambda (x) (file-name-as-directory x)) load-path)) (setq load-path (cons new-path load-path))))

(autoload 'update-autoloads-in-package-area "update-auto-loads" "\
Update autoloads for files in the directory containing this file.
Add a file named `.cedet' at the root of directory where `cedet-update-autoloads'
must be used to generate the `loaddefs.el''s autoloads for itself and its
subdirectories instead of the standard `update-autoloads-from-directories'.

\(fn &optional FILE)" t nil)

(autoload 'update-autoloads-for-file-in-package-area "update-auto-loads" "\
Not documented

\(fn FILE)" t nil)

;;;***

;;;### (autoloads (vcard-parse-region vcard-parse-string vcard-pretty-print
;;;;;;  vcard-standard-filters vcard-pretty-print-function) "vcard"
;;;;;;  "vcard.el" (19826 11393))
;;; Generated autoloads from vcard.el

(defvar vcard-pretty-print-function 'vcard-format-sample-box "\
*Formatting function used by `vcard-pretty-print'.")

(custom-autoload 'vcard-pretty-print-function "vcard" t)

(defvar vcard-standard-filters '(vcard-filter-html vcard-filter-adr-newlines vcard-filter-tel-normalize vcard-filter-textprop-cr) "\
*Standard list of filters to apply to parsed vcard data.
These filters are applied sequentially to vcard attributes when
the function `vcard-standard-filter' is supplied as the second argument to
`vcard-parse'.")

(custom-autoload 'vcard-standard-filters "vcard" t)

(autoload 'vcard-pretty-print "vcard" "\
Format VCARD into a string suitable for display to user.
VCARD can be an unparsed string containing raw VCF vcard data
or a parsed vcard alist as returned by `vcard-parse-string'.

The result is a string with formatted vcard information suitable for
insertion into a mime presentation buffer.

The function specified by the variable `vcard-pretty-print-function'
actually performs the formatting.  That function will always receive a
parsed vcard alist.

\(fn VCARD)" nil nil)

(autoload 'vcard-parse-string "vcard" "\
Parse RAW vcard data as a string, and return an alist representing data.

If the optional function FILTER is specified, apply that filter to each
attribute.  If no filter is specified, `vcard-standard-filter' is used.

Filters should accept two arguments: the property list and the value list.
Modifying in place the property or value list will affect the resulting
attribute in the vcard alist.

Vcard data is normally in the form

    begin:                        vcard
    prop1a:                       value1a
    prop2a;prop2b;prop2c=param2c: value2a
    prop3a;prop3b:                value3a;value3b;value3c
    end:                          vcard

\(Whitespace around the `:' separating properties and values is optional.)
If supplied to this function an alist of the form

    (((\"prop1a\") \"value1a\")
     ((\"prop2a\" \"prop2b\" (\"prop2c\" . \"param2c\")) \"value2a\")
     ((\"prop3a\" \"prop3b\") \"value3a\" \"value3b\" \"value3c\"))

would be returned.

\(fn RAW &optional FILTER)" nil nil)

(autoload 'vcard-parse-region "vcard" "\
Parse the raw vcard data in region, and return an alist representing data.
This function is just like `vcard-parse-string' except that it operates on
a region of the current buffer rather than taking a string as an argument.

Note: this function modifies the buffer!

\(fn BEG END &optional FILTER)" nil nil)

;;;***

;;;### (autoloads (viper-mode toggle-viper-mode) "viper" "viper.el"
;;;;;;  (19811 54831))
;;; Generated autoloads from viper.el

(autoload 'toggle-viper-mode "viper" "\
Toggle Viper on/off.
If Viper is enabled, turn it off.  Otherwise, turn it on.

\(fn)" t nil)

(autoload 'viper-mode "viper" "\
Turn on Viper emulation of Vi in Emacs. See Info node `(viper)Top'.

\(fn)" t nil)

;;;***

;;;### (autoloads (vline-global-mode vline-mode) "vline" "vline.el"
;;;;;;  (19804 63445))
;;; Generated autoloads from vline.el

(autoload 'vline-mode "vline" "\
Display vertical line mode.

\(fn &optional ARG)" t nil)

(defvar vline-global-mode nil "\
Non-nil if Vline-Global mode is enabled.
See the command `vline-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vline-global-mode'.")

(custom-autoload 'vline-global-mode "vline" nil)

(autoload 'vline-global-mode "vline" "\
Toggle Vline mode in every possible buffer.
With prefix ARG, turn Vline-Global mode on if and only if
ARG is positive.
Vline mode is enabled in all buffers where
`(lambda nil (unless (minibufferp) (vline-mode 1)))' would do it.
See `vline-mode' for more information on Vline mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (volume) "volume" "volume.el" (18755 34113))
;;; Generated autoloads from volume.el

(autoload 'volume "volume" "\
Tweak your sound card volume.

\(fn)" t nil)

;;;***

;;;### (autoloads (w3m-buffer w3m-region w3m-find-file w3m-browse-url
;;;;;;  w3m w3m-gohome w3m-goto-url-new-session w3m-goto-url w3m-download
;;;;;;  w3m-retrieve) "w3m" "emacs-w3m/w3m.el" (19100 32629))
;;; Generated autoloads from emacs-w3m/w3m.el

(autoload 'w3m-retrieve "w3m" "\
Retrieve web contents pointed to by URL.
It will put the retrieved contents into the current buffer.

If HANDLER is nil, this function will retrieve web contents, return
the content type of the retrieved data, and then come to an end.  This
behavior is what is called a synchronous operation.  You have to
specify HANDLER in order to make this function show its real ability,
which is called an asynchronous operation.

If HANDLER is a function, this function will come to an end in no time.
In this case, contents will be retrieved by the asynchronous process
after a while.  And after finishing retrieving contents successfully,
HANDLER will be called on the buffer where this function starts.  The
content type of the retrieved data will be passed to HANDLER as a
string argument.

NO-UNCOMPRESS specifies whether this function should not uncompress contents.
NO-CACHE specifies whether this function should not use cached contents.
POST-DATA and REFERER will be sent to the web server with a request.

\(fn URL &optional NO-UNCOMPRESS NO-CACHE POST-DATA REFERER HANDLER)" nil nil)

(autoload 'w3m-download "w3m" "\
Download contents of URL to a file named FILENAME.
NO-CHACHE (which the prefix argument gives when called interactively)
specifies not using the cached data.

\(fn URL &optional FILENAME NO-CACHE HANDLER POST-DATA)" t nil)

(autoload 'w3m-goto-url "w3m" "\
Visit World Wide Web pages.  This is the primitive function of `w3m'.
If the second argument RELOAD is non-nil, reload a content of URL.
Except that if it is 'redisplay, re-display the page without reloading.
The third argument CHARSET specifies a charset to be used for decoding
a content.
The fourth argument POST-DATA should be a string or a cons cell.  If
it is a string, it makes this function request a body as if the
content-type is \"x-www-form-urlencoded\".  If it is a cons cell, the
car of a cell is used as the content-type and the cdr of a cell is
used as the body.
If the fifth argument REFERER is specified, it is used for a Referer:
field for this request.
The remaining HANDLER, ELEMENT[1], and NO-POPUP are for the
internal operations of emacs-w3m.
You can also use \"quicksearch\" url schemes such as \"gg:emacs\" which
would search for the term \"emacs\" with the Google search engine.  See
the `w3m-search' function and the variable `w3m-uri-replace-alist'.

\[1] A note for the developers: ELEMENT is a history element which has
already been registered in the `w3m-history-flat' variable.  It is
corresponding to URL to be retrieved at this time, not for the url of
the current page.

\(fn URL &optional RELOAD CHARSET POST-DATA REFERER HANDLER ELEMENT NO-POPUP)" t nil)

(autoload 'w3m-goto-url-new-session "w3m" "\
Visit World Wide Web pages in a new session.
If you invoke this command in the emacs-w3m buffer, the new session
will be created by copying the current session.  Otherwise, the new
session will start afresh.

\(fn URL &optional RELOAD CHARSET POST-DATA REFERER)" t nil)

(autoload 'w3m-gohome "w3m" "\
Go to the Home page.

\(fn)" t nil)

(autoload 'w3m "w3m" "\
Visit World Wide Web pages using the external w3m command.

When you invoke this command interactively for the first time, it will
visit a page which is pointed to by a string like url around the
cursor position or the home page specified by the `w3m-home-page'
variable, but you will be prompted for a URL if `w3m-quick-start' is
nil (default t) or `w3m-home-page' is nil.

The variables `w3m-pop-up-windows' and `w3m-pop-up-frames' control
whether this command should pop to a window or a frame up for the
session.

When emacs-w3m sessions have already been opened, this command will
pop to the existing window or frame up, but if `w3m-quick-start' is
nil, (default t), you will be prompted for a URL (which defaults to
`popup' meaning to pop to an existing emacs-w3m buffer up).

In addition, if the prefix argument is given or you enter the empty
string for the prompt, it will visit the home page specified by the
`w3m-home-page' variable or the \"about:\" page.

You can also run this command in the batch mode as follows:

  emacs -f w3m http://emacs-w3m.namazu.org/ &

In that case, or if this command is called non-interactively, the
variables `w3m-pop-up-windows' and `w3m-pop-up-frames' will be ignored
\(treated as nil) and it will run emacs-w3m at the current (or the
initial) window.

If the optional NEW-SESSION is non-nil, this function makes a new
emacs-w3m buffer.  Besides that, it also makes a new emacs-w3m buffer
if `w3m-make-new-session' is non-nil and a user specifies a url string.

The optional INTERACTIVE-P is for the internal use; it is mainly used
to check whether Emacs 22 or later calls this function as an
interactive command in the batch mode.

\(fn &optional URL NEW-SESSION INTERACTIVE-P)" t nil)

(autoload 'w3m-browse-url "w3m" "\
Ask emacs-w3m to browse URL.
NEW-SESSION specifies whether to create a new emacs-w3m session.  URL
defaults to the string looking like a url around the cursor position.
Pop to a window or a frame up according to `w3m-pop-up-windows' and
`w3m-pop-up-frames'.

\(fn URL &optional NEW-SESSION)" t nil)

(autoload 'w3m-find-file "w3m" "\
Function used to open FILE whose name is expressed in ordinary format.
The file name will be converted into the file: scheme.

\(fn FILE)" t nil)

(autoload 'w3m-region "w3m" "\
Render the region of the current buffer between START and END.
URL specifies the address where the contents come from.  It can be
omitted or nil when the address is not identified.  CHARSET is used
for decoding the contents.  If it is nil, this function attempts to
parse the meta tag to extract the charset.

\(fn START END &optional URL CHARSET)" t nil)

(autoload 'w3m-buffer "w3m" "\
Render the current buffer.
See `w3m-region' for the optional arguments.

\(fn &optional URL CHARSET)" t nil)

;;;***

;;;### (autoloads (w3m-antenna w3m-about-antenna) "w3m-antenna" "emacs-w3m/w3m-antenna.el"
;;;;;;  (18712 53399))
;;; Generated autoloads from emacs-w3m/w3m-antenna.el

(autoload 'w3m-about-antenna "w3m-antenna" "\
Not documented

\(fn URL &optional NO-DECODE NO-CACHE POST-DATA REFERER HANDLER)" nil nil)

(autoload 'w3m-antenna "w3m-antenna" "\
Report changes of WEB sites, which is specified in `w3m-antenna-sites'.

\(fn &optional NO-CACHE)" t nil)

;;;***

;;;### (autoloads (w3m-setup-bookmark-menu w3m-about-bookmark w3m-bookmark-view-new-session
;;;;;;  w3m-bookmark-view w3m-bookmark-add-current-url-group w3m-bookmark-add-all-urls
;;;;;;  w3m-bookmark-add-current-url w3m-bookmark-add-this-url) "w3m-bookmark"
;;;;;;  "emacs-w3m/w3m-bookmark.el" (18964 36505))
;;; Generated autoloads from emacs-w3m/w3m-bookmark.el

(autoload 'w3m-bookmark-add-this-url "w3m-bookmark" "\
Add link under cursor to bookmark.

\(fn)" t nil)

(autoload 'w3m-bookmark-add-current-url "w3m-bookmark" "\
Add a url of the current page to the bookmark.
With prefix, ask for a new url instead of the present one.

\(fn &optional ARG)" t nil)

(autoload 'w3m-bookmark-add-all-urls "w3m-bookmark" "\
Add urls of all pages being visited to the bookmark.

\(fn)" t nil)

(autoload 'w3m-bookmark-add-current-url-group "w3m-bookmark" "\
Add link of the group of current urls to the bookmark.

\(fn)" t nil)

(autoload 'w3m-bookmark-view "w3m-bookmark" "\
Display the bookmark.

\(fn &optional RELOAD)" t nil)

(autoload 'w3m-bookmark-view-new-session "w3m-bookmark" "\
Display the bookmark on a new session.

\(fn &optional RELOAD)" t nil)

(autoload 'w3m-about-bookmark "w3m-bookmark" "\
Not documented

\(fn &rest ARGS)" nil nil)

(autoload 'w3m-setup-bookmark-menu "w3m-bookmark" "\
Setup w3m bookmark items in menubar.

\(fn)" nil nil)

;;;***

;;;### (autoloads (w3m-about-cookie w3m-cookie w3m-cookie-get w3m-cookie-set
;;;;;;  w3m-cookie-shutdown) "w3m-cookie" "emacs-w3m/w3m-cookie.el"
;;;;;;  (18964 36505))
;;; Generated autoloads from emacs-w3m/w3m-cookie.el

(autoload 'w3m-cookie-shutdown "w3m-cookie" "\
Save cookies, and reset cookies' data.

\(fn)" t nil)

(autoload 'w3m-cookie-set "w3m-cookie" "\
Register cookies which correspond to URL.
BEG and END should be an HTTP response header region on current buffer.

\(fn URL BEG END)" nil nil)

(autoload 'w3m-cookie-get "w3m-cookie" "\
Get a cookie field string which corresponds to the URL.

\(fn URL)" nil nil)

(autoload 'w3m-cookie "w3m-cookie" "\
Display cookies and enable you to manage them.

\(fn &optional NO-CACHE)" t nil)

(autoload 'w3m-about-cookie "w3m-cookie" "\
Make the html contents to display and to enable you to manage cookies.

\(fn URL &optional NO-DECODE NO-CACHE POST-DATA &rest ARGS)" nil nil)

;;;***

;;;### (autoloads (w3m-dtree w3m-about-dtree) "w3m-dtree" "emacs-w3m/w3m-dtree.el"
;;;;;;  (18964 36505))
;;; Generated autoloads from emacs-w3m/w3m-dtree.el

(autoload 'w3m-about-dtree "w3m-dtree" "\
Not documented

\(fn URL &optional NODECODE ALLFILES &rest ARGS)" nil nil)

(autoload 'w3m-dtree "w3m-dtree" "\
Display directory tree on local file system.
If called with 'prefix argument', display all directorys and files.

\(fn ALLFILES PATH)" t nil)

;;;***

;;;### (autoloads (w3m-fb-mode) "w3m-fb" "w3m-fb.el" (19833 6366))
;;; Generated autoloads from w3m-fb.el

(defvar w3m-fb-mode nil "\
Non-nil if W3m-Fb mode is enabled.
See the command `w3m-fb-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `w3m-fb-mode'.")

(custom-autoload 'w3m-fb-mode "w3m-fb" nil)

(autoload 'w3m-fb-mode "w3m-fb" "\
Toggle W3M Frame Buffer mode.
This allows frame-local lists of buffers (tabs).

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (w3m-filter) "w3m-filter" "emacs-w3m/w3m-filter.el"
;;;;;;  (18560 15090))
;;; Generated autoloads from emacs-w3m/w3m-filter.el

(autoload 'w3m-filter "w3m-filter" "\
Apply filtering rule of URL against a content in this buffer.

\(fn URL)" nil nil)

;;;***

;;;### (autoloads (w3m-fontify-forms) "w3m-form" "emacs-w3m/w3m-form.el"
;;;;;;  (18964 36505))
;;; Generated autoloads from emacs-w3m/w3m-form.el

(autoload 'w3m-fontify-forms "w3m-form" "\
Process half-dumped data and fontify forms in this buffer.

\(fn)" nil nil)

;;;***

;;;### (autoloads (w3m-link-numbering-mode) "w3m-lnum" "emacs-w3m/w3m-lnum.el"
;;;;;;  (18851 39583))
;;; Generated autoloads from emacs-w3m/w3m-lnum.el

(autoload 'w3m-link-numbering-mode "w3m-lnum" "\
Minor mode to enable operations using link numbers.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (w3m-namazu w3m-about-namazu) "w3m-namazu" "emacs-w3m/w3m-namazu.el"
;;;;;;  (18964 36505))
;;; Generated autoloads from emacs-w3m/w3m-namazu.el

(autoload 'w3m-about-namazu "w3m-namazu" "\
Not documented

\(fn URL &optional NO-DECODE NO-CACHE &rest ARGS)" nil nil)

(autoload 'w3m-namazu "w3m-namazu" "\
Search indexed files with Namazu.

\(fn INDEX QUERY &optional RELOAD)" t nil)

;;;***

;;;### (autoloads (w3m-perldoc w3m-about-perldoc) "w3m-perldoc" "emacs-w3m/w3m-perldoc.el"
;;;;;;  (18197 61165))
;;; Generated autoloads from emacs-w3m/w3m-perldoc.el

(autoload 'w3m-about-perldoc "w3m-perldoc" "\
Not documented

\(fn URL &optional NO-DECODE NO-CACHE &rest ARGS)" nil nil)

(autoload 'w3m-perldoc "w3m-perldoc" "\
View Perl documents.

\(fn DOCNAME)" t nil)

;;;***

;;;### (autoloads (w3m-search-uri-replace w3m-search-new-session
;;;;;;  w3m-search) "w3m-search" "emacs-w3m/w3m-search.el" (19101
;;;;;;  52640))
;;; Generated autoloads from emacs-w3m/w3m-search.el

(autoload 'w3m-search "w3m-search" "\
Search QUERY using SEARCH-ENGINE.
When called interactively with a prefix argument, you can choose one of
the search engines defined in `w3m-search-engine-alist'.  Otherwise use
`w3m-search-default-engine'.
If Transient Mark mode, use the region as an initial string of query
and deactivate the mark.

\(fn SEARCH-ENGINE QUERY)" t nil)

(autoload 'w3m-search-new-session "w3m-search" "\
Like `w3m-search', but do the search in a new session.

\(fn SEARCH-ENGINE QUERY)" t nil)

(autoload 'w3m-search-uri-replace "w3m-search" "\
Generate query string for ENGINE from URI matched by last search.

\(fn URI ENGINE)" nil nil)

;;;***

;;;### (autoloads (w3m-session-last-crashed-session w3m-session-last-autosave-session
;;;;;;  w3m-setup-session-menu w3m-session-select w3m-session-save)
;;;;;;  "w3m-session" "emacs-w3m/w3m-session.el" (18964 36505))
;;; Generated autoloads from emacs-w3m/w3m-session.el

(autoload 'w3m-session-save "w3m-session" "\
Save list of displayed session.

\(fn)" t nil)

(autoload 'w3m-session-select "w3m-session" "\
Select session from session list.

\(fn)" t nil)

(autoload 'w3m-setup-session-menu "w3m-session" "\
Setup w3m session items in menubar.

\(fn)" nil nil)

(autoload 'w3m-session-last-autosave-session "w3m-session" "\
Not documented

\(fn)" nil nil)

(autoload 'w3m-session-last-crashed-session "w3m-session" "\
Not documented

\(fn)" nil nil)

;;;***

;;;### (autoloads (w3m-replace-symbol) "w3m-symbol" "emacs-w3m/w3m-symbol.el"
;;;;;;  (18791 2971))
;;; Generated autoloads from emacs-w3m/w3m-symbol.el

(autoload 'w3m-replace-symbol "w3m-symbol" "\
Not documented

\(fn)" nil nil)

;;;***

;;;### (autoloads (w3m-about-weather w3m-weather) "w3m-weather" "emacs-w3m/w3m-weather.el"
;;;;;;  (18197 61165))
;;; Generated autoloads from emacs-w3m/w3m-weather.el

(autoload 'w3m-weather "w3m-weather" "\
Display weather report.

\(fn AREA)" t nil)

(autoload 'w3m-about-weather "w3m-weather" "\
Not documented

\(fn URL NO-DECODE NO-CACHE POST-DATA REFERER HANDLER)" nil nil)

;;;***

;;;### (autoloads (which-function-mode) "which-func" "rinari/util/jump/which-func.el"
;;;;;;  (19809 16109))
;;; Generated autoloads from rinari/util/jump/which-func.el
 (put 'which-func-format 'risky-local-variable t)
 (put 'which-func-current 'risky-local-variable t)

(defalias 'which-func-mode 'which-function-mode)

(defvar which-function-mode nil "\
Non-nil if Which-Function mode is enabled.
See the command `which-function-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `which-function-mode'.")

(custom-autoload 'which-function-mode "which-func" nil)

(autoload 'which-function-mode "which-func" "\
Toggle Which Function mode, globally.
When Which Function mode is enabled, the current function name is
continuously displayed in the mode line, in certain major modes.

With prefix ARG, turn Which Function mode on if arg is positive,
and off otherwise.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (window-numbering-mode) "window-numbering" "window-numbering.el"
;;;;;;  (18901 61584))
;;; Generated autoloads from window-numbering.el

(defvar window-numbering-mode nil "\
Non-nil if Window-Numbering mode is enabled.
See the command `window-numbering-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `window-numbering-mode'.")

(custom-autoload 'window-numbering-mode "window-numbering" nil)

(autoload 'window-numbering-mode "window-numbering" "\
A minor mode that assigns a number to each window.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (resume-windows see-you-again win-load-all-configurations
;;;;;;  wipe-windows win-save-all-configurations win:startup-with-window
;;;;;;  win:set-wc win-switch-to-window) "windows" "windows.el" (19828
;;;;;;  49883))
;;; Generated autoloads from windows.el

(autoload 'win-switch-to-window "windows" "\
Switch window configurations to a buffer specified by keyboard.
If calling from program, optional second argument WINDOW can specify
the window number.

\(fn ARG &optional WINDOW)" t nil)

(autoload 'win:set-wc "windows" "\
(Windows low level internal) Set the NUM-th windows configuration.
If Windows uses frame(Emacs 19), Select the NUM-th window frame.

\(fn NUM)" nil nil)

(autoload 'win:startup-with-window "windows" "\
Start up Emacs with window[1] selected.

\(fn)" nil nil)

(autoload 'win-save-all-configurations "windows" "\
Save all window configurations into the configuration file.

\(fn)" t nil)

(autoload 'wipe-windows "windows" "\
Kill all buffers.  Optional argument NO-ASK non-nil skips query.

\(fn &optional NO-ASK)" t nil)

(autoload 'win-load-all-configurations "windows" "\
Load all window configurations from the configuration file.
Non-nil for optional argument PRESERVE keeps all current buffers.

\(fn &optional PRESERVE)" t nil)

(autoload 'see-you-again "windows" "\
Save all of the window configurations and kill-emacs.

\(fn)" t nil)

(autoload 'resume-windows "windows" "\
Restore all window configurations reading configurations from a file.
Non-nil for optional argument PRESERVE keeps current buffers.

\(fn &optional PRESERVE)" t nil)

;;;***

;;;### (autoloads (winring-rename-configuration winring-delete-configuration
;;;;;;  winring-jump-to-configuration winring-prev-configuration
;;;;;;  winring-next-configuration winring-duplicate-configuration
;;;;;;  winring-new-configuration) "winring" "winring.el" (19386
;;;;;;  63009))
;;; Generated autoloads from winring.el

(autoload 'winring-new-configuration "winring" "\
Save the current window configuration and create an empty new one.
The buffer shown in the new empty configuration is defined by
`winring-new-config-buffer-name'.

With \\[universal-argument] prompt for the new configuration's name.
Otherwise, the function in `winring-name-generator' will be called to
get the new configuration's name.

\(fn &optional ARG)" t nil)

(autoload 'winring-duplicate-configuration "winring" "\
Push the current window configuration on the ring, and duplicate it.

With \\[universal-argument] prompt for the new configuration's name.
Otherwise, the function in `winring-name-generator' will be called to
get the new configuration's name.

\(fn &optional ARG)" t nil)

(autoload 'winring-next-configuration "winring" "\
Switch to the next window configuration for this frame.

\(fn)" t nil)

(autoload 'winring-prev-configuration "winring" "\
Switch to the previous window configuration for this frame.

\(fn)" t nil)

(autoload 'winring-jump-to-configuration "winring" "\
Go to the named window configuration.

\(fn)" t nil)

(autoload 'winring-delete-configuration "winring" "\
Delete the current configuration and switch to the next one.
With \\[universal-argument] prompt for named configuration to delete.

\(fn &optional ARG)" t nil)

(autoload 'winring-rename-configuration "winring" "\
Rename the current configuration to NAME.

\(fn NAME)" t nil)

;;;***

;;;### (autoloads (wisent-parse-toggle-verbose-flag) "wisent" "cedet/semantic/wisent/wisent.el"
;;;;;;  (19390 35817))
;;; Generated autoloads from cedet/semantic/wisent/wisent.el

(defvar wisent-parse-verbose-flag nil "\
*Non-nil means to issue more messages while parsing.")

(autoload 'wisent-parse-toggle-verbose-flag "wisent" "\
Toggle whether to issue more messages while parsing.

\(fn)" t nil)

;;;***

;;;### (autoloads (wisent-c-default-setup) "wisent-c" "cedet/semantic/wisent/wisent-c.el"
;;;;;;  (19070 11963))
;;; Generated autoloads from cedet/semantic/wisent/wisent-c.el

(autoload 'wisent-c-default-setup "wisent-c" "\
Setup buffer for parse.

\(fn)" nil nil)

;;;***

;;;### (autoloads (wisent-calc-utest wisent-calc) "wisent-calc" "cedet/semantic/wisent/wisent-calc.el"
;;;;;;  (19390 35849))
;;; Generated autoloads from cedet/semantic/wisent/wisent-calc.el

(autoload 'wisent-calc "wisent-calc" "\
Infix desktop calculator.
Parse INPUT string and output the result of computation.

\(fn INPUT)" t nil)

(autoload 'wisent-calc-utest "wisent-calc" "\
Test the wisent calculator.

\(fn)" t nil)

;;;***

;;;### (autoloads (wisent-byte-compile-grammar wisent-compile-grammar
;;;;;;  wisent-toggle-verbose-flag) "wisent-comp" "cedet/semantic/wisent/wisent-comp.el"
;;;;;;  (19390 35899))
;;; Generated autoloads from cedet/semantic/wisent/wisent-comp.el

(defvar wisent-verbose-flag nil "\
*Non-nil means to report verbose information on generated parser.")

(autoload 'wisent-toggle-verbose-flag "wisent-comp" "\
Toggle whether to report verbose information on generated parser.

\(fn)" t nil)

(autoload 'wisent-compile-grammar "wisent-comp" "\
Compile the LALR(1) GRAMMAR.

GRAMMAR is a list (TOKENS ASSOCS . NONTERMS) where:

- TOKENS is a list of terminal symbols (tokens).

- ASSOCS is nil, or an alist of (ASSOC-TYPE . ASSOC-VALUE) elements
  describing the associativity of TOKENS.  ASSOC-TYPE must be one of
  the `default-prec' `nonassoc', `left' or `right' symbols.  When
  ASSOC-TYPE is `default-prec', ASSOC-VALUE must be nil or t (the
  default).  Otherwise it is a list of tokens which must have been
  previously declared in TOKENS.

- NONTERMS is a list of nonterminal definitions.

Optional argument START-LIST specify the possible grammar start
symbols.  This is a list of nonterminals which must have been
previously declared in GRAMMAR's NONTERMS form.  By default, the start
symbol is the first nonterminal defined.  When START-LIST contains
only one element, it is the start symbol.  Otherwise, all elements are
possible start symbols, unless `wisent-single-start-flag' is non-nil.
In that case, the first element is the start symbol, and others are
ignored.

Return an automaton as a vector: [ACTIONS GOTOS STARTS FUNCTIONS]
where:

- ACTIONS is a state/token matrix telling the parser what to do at
  every state based on the current lookahead token.  That is shift,
  reduce, accept or error.

- GOTOS is a state/nonterminal matrix telling the parser the next
  state to go to after reducing with each rule.

- STARTS is an alist which maps the allowed start nonterminal symbols
  to tokens that will be first shifted into the parser stack.

- FUNCTIONS is an obarray of semantic action symbols.  Each symbol's
  function definition is the semantic action lambda expression.

\(fn GRAMMAR &optional START-LIST)" nil nil)

(autoload 'wisent-byte-compile-grammar "wisent-comp" "\
Byte compile the `wisent-compile-grammar' FORM.
Automatically called by the Emacs Lisp byte compiler as a
`byte-compile' handler.

\(fn FORM)" nil nil)

(put 'wisent-compile-grammar 'byte-compile 'wisent-byte-compile-grammar)

;;;***

;;;### (autoloads (wisent-csharp-default-setup) "wisent-csharp" "cedet/contrib/wisent-csharp.el"
;;;;;;  (19522 62479))
;;; Generated autoloads from cedet/contrib/wisent-csharp.el

(autoload 'wisent-csharp-default-setup "wisent-csharp" "\
Not documented

\(fn)" nil nil)

(add-hook 'csharp-mode-hook #'wisent-csharp-default-setup)

;;;***

;;;### (autoloads (wisent-debug-show-entry wisent-cancel-debug-on-entry
;;;;;;  wisent-debug-on-entry) "wisent-debug" "cedet/semantic/wisent/wisent-debug.el"
;;;;;;  (17881 43106))
;;; Generated autoloads from cedet/semantic/wisent/wisent-debug.el

(autoload 'wisent-debug-on-entry "wisent-debug" "\
Request AUTOMATON's FUNCTION to invoke debugger each time it is called.
FUNCTION must be a semantic action symbol that exists in AUTOMATON.

\(fn AUTOMATON FUNCTION)" t nil)

(autoload 'wisent-cancel-debug-on-entry "wisent-debug" "\
Undo effect of \\[wisent-debug-on-entry] on AUTOMATON's FUNCTION.
FUNCTION must be a semantic action symbol that exists in AUTOMATON.

\(fn AUTOMATON FUNCTION)" t nil)

(autoload 'wisent-debug-show-entry "wisent-debug" "\
Show the source of AUTOMATON's semantic action FUNCTION.
FUNCTION must be a semantic action symbol that exists in AUTOMATON.

\(fn AUTOMATON FUNCTION)" t nil)

;;;***

;;;### (autoloads (wisent-dot-setup-parser) "wisent-dot" "cedet/cogre/wisent-dot.el"
;;;;;;  (18906 40873))
;;; Generated autoloads from cedet/cogre/wisent-dot.el

(autoload 'wisent-dot-setup-parser "wisent-dot" "\
Setup buffer for parse.

\(fn)" nil nil)

(add-hook 'graphviz-dot-mode-hook 'wisent-dot-setup-parser)

(add-hook 'cogre-dot-mode-hook 'wisent-dot-setup-parser)

;;;***

;;;### (autoloads (wisent-grammar-mode) "wisent-grammar" "cedet/semantic/wisent/wisent-grammar.el"
;;;;;;  (17213 40639))
;;; Generated autoloads from cedet/semantic/wisent/wisent-grammar.el

(autoload 'wisent-grammar-mode "wisent-grammar" "\
Major mode for editing Wisent grammars.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.wy$" . wisent-grammar-mode))

(eval-after-load "speedbar" '(speedbar-add-supported-extension ".wy"))

;;;***

;;;### (autoloads (wisent-java-default-setup) "wisent-java-tags"
;;;;;;  "cedet/semantic/wisent/wisent-java-tags.el" (19358 14567))
;;; Generated autoloads from cedet/semantic/wisent/wisent-java-tags.el

(autoload 'wisent-java-default-setup "wisent-java-tags" "\
Hook run to setup Semantic in `java-mode'.
Use the alternate LALR(1) parser.

\(fn)" nil nil)

(add-hook 'java-mode-hook 'wisent-java-default-setup)

;;;***

;;;### (autoloads (wisent-javascript-setup-parser) "wisent-javascript"
;;;;;;  "cedet/semantic/wisent/wisent-javascript.el" (19575 43475))
;;; Generated autoloads from cedet/semantic/wisent/wisent-javascript.el

(autoload 'wisent-javascript-setup-parser "wisent-javascript" "\
Setup buffer for parse.

\(fn)" nil nil)

(add-hook 'javascript-mode-hook 'wisent-javascript-setup-parser)

(add-hook 'ecmascript-mode-hook 'wisent-javascript-setup-parser)

(add-hook 'js-mode-hook 'wisent-javascript-setup-parser)

(add-hook 'js2-mode-hook 'wisent-javascript-setup-parser)

;;;***

;;;### (autoloads (wisent-php-default-setup) "wisent-php" "cedet/contrib/wisent-php.el"
;;;;;;  (18810 37948))
;;; Generated autoloads from cedet/contrib/wisent-php.el

(autoload 'wisent-php-default-setup "wisent-php" "\
Hook run to setup Semantic in `php-mode'.
Use the alternate LALR(1) parser.

\(fn)" nil nil)

(add-hook 'php-mode-hook #'wisent-php-default-setup)

;;;***

;;;### (autoloads (wisent-python-default-setup) "wisent-python" "cedet/semantic/wisent/wisent-python.el"
;;;;;;  (19373 12958))
;;; Generated autoloads from cedet/semantic/wisent/wisent-python.el

(autoload 'wisent-python-default-setup "wisent-python" "\
Setup buffer for parse.

\(fn)" nil nil)

(add-hook 'python-mode-hook 'wisent-python-default-setup)

;;;***

;;;### (autoloads (global-ws-trim-mode ws-trim-mode turn-on-ws-trim
;;;;;;  ws-trim-buffer ws-trim-region ws-trim-line) "ws-trim" "ws-trim.el"
;;;;;;  (19832 56422))
;;; Generated autoloads from ws-trim.el

(defvar ws-trim-method-hook 'ws-trim-trailing "\
*The kind of trimming done by the WS Trim mode and functions.
A single or a list of functions which are run on each line that's
getting trimmed.  Supplied trim functions:

`ws-trim-trailing'        Delete trailing whitespace.
`ws-trim-leading-spaces'  Replace unnecessary leading spaces with tabs.
`ws-trim-leading-tabs'    Replace leading tabs with spaces.
`ws-trim-tabs'            Replace all tabs with spaces.

This is a perfectly normal hook run by `run-hooks' and custom
functions can of course be used.  There's no inherent restriction to
just whitespace trimming either, for that matter.  Each function
should modify the current line and leave point somewhere on it.")

(autoload 'ws-trim-line "ws-trim" "\
Trim whitespace on the current line.
Do this according to the hook `ws-trim-method-hook'.  With a prefix
argument, ask for the trim method to use instead.

\(fn ARG)" t nil)

(autoload 'ws-trim-region "ws-trim" "\
Trim whitespace on each line in the region.
Do this according to the hook `ws-trim-method-hook'.  With a prefix
argument, ask for the trim method to use instead.

\(fn ARG)" t nil)

(autoload 'ws-trim-buffer "ws-trim" "\
Trim whitespace on each line in the buffer.
Do this according to the hook `ws-trim-method-hook'.  With a prefix
argument, ask for the trim method to use instead.

\(fn ARG)" t nil)

(defvar ws-trim-mode nil "\
If non-nil, WS Trim mode is active.
This mode automatically trims whitespace on text lines.  The kind of
trimming is specified by the hook `ws-trim-method-hook'.  You can
either trim every line in the buffer or just the lines you edit
manually, see the variable `ws-trim-level' for details.  This mode
runs the hook `ws-trim-mode-hook' when activated.

Please note that there are other common functions, e.g. `indent-to',
`newline-and-indent' (often bound to LFD or RET), `fill-paragraph',
and the variable `indent-tabs-mode', that also trims whitespace in
various circumstances.  They are entirely independent of this mode.

To automatically enable WS Trim mode in any major mode, put
`turn-on-ws-trim' in the major mode's hook, e.g. in your .emacs:

  (add-hook 'emacs-lisp-mode-hook 'turn-on-ws-trim)

You can also activate WS Trim mode automagically in all modes where
it's likely to be useful by putting the following in .emacs:

  (global-ws-trim-mode t)

Exactly when WS Trim is activated are by default controlled by a
heuristic, see the function `ws-trim-mode-heuristic' for details.  You
can get more control over the process through the variable
`global-ws-trim-modes'.

This variable automatically becomes buffer local when modified.  It
should not be set directly; use the commands `ws-trim-mode' or
`turn-on-ws-trim' instead.")

(defvar ws-trim-level 0 "\
*How thorough automatic whitespace trimming should be in WS Trim mode.
If 3 or greater, all lines in the buffer are kept trimmed at all
times (if the buffer is modifiable).
If 2, all lines in the buffer are trimmed when the buffer is modified
for the first time.
If 1, only modified lines are trimmed.
If 0, only single modified lines are trimmed, i.e. operations that
modify more than one line doesn't cause any trimming (newline is an
exception).

The current line is never trimmed on any level, unless the buffer is
about to be written.  In that case the current line is treated as any
other line.

The default level is 0, which is very restrictive.  This is
particularly useful when you edit files which are compared with diff
\(e.g. for patches), because parts that you don't change manually are
kept unchanged.  You can also do block operations over several lines
without risking strange side effects (e.g. paste patches into mails).

This variable automatically becomes buffer local when changed.  Use
the function `set-default' to set the value it defaults to in all new
buffers.  If you want even more control it's best to put a suitable
function onto `ws-trim-mode-hook'.  Changes of `ws-trim-level' might
not take effect immediately; it's best set when the mode is
initialized.")

(defvar ws-trim-mode-line-string " Trim" "\
*Modeline string for WS Trim mode.
Set to nil to remove the modeline indicator for ws-trim.")

(defvar ws-trim-mode-hook nil "\
A normal hook which is run when WS Trim mode is turned on.
This hook is run by `run-hooks' and can therefore be buffer local.

Some care might be necessary when putting functions on this hook due
to the somewhat strange circumstances under which it's run.
Specifically, anything put here might indirectly be run from
`post-command-hook' or `find-file-hooks'.  Don't worry about it if you
just want to do something simple, e.g. setting some variables.")

(autoload 'turn-on-ws-trim "ws-trim" "\
Unconditionally turn on WS Trim mode.
See the variable `ws-trim-mode' for further info on this mode.

\(fn)" t nil)

(autoload 'ws-trim-mode "ws-trim" "\
Toggle WS Trim mode, which automatically trims whitespace on lines.
A positive prefix argument turns the mode on, any other prefix turns
it off.

See the variable docstring for details about this mode.

\(fn &optional ARG)" t nil)

(defvar global-ws-trim-mode nil "\
If non-nil, automagically turn on WS Trim mode in many major modes.
How it's done is controlled by the variable `ws-trim-global-modes'.

This variable should not be changed directly; use the command
`global-ws-trim-mode' instead.")

(defvar ws-trim-global-modes 'guess "\
*Controls which major modes should have WS Trim mode turned on.
Global WS Trim mode must first be activated, which is done by the
command `global-ws-trim-mode'.

If nil, no modes turn on WS Trim mode.
If t, all modes turn on WS Trim mode.
If `guess', then a heuristic is used to determine whether WS Trim mode
should be activated in the mode in question.  See
`ws-trim-mode-heuristic' for details.
If a list, then all modes whose `major-mode' symbol names matches some
entry in it turn on WS Trim mode.
If a list begins with `not', all modes but the ones mentioned turn on
WS Trim mode.
If a list begins with `guess', then the remaining elements must in
turn be lists as above.  All modes not specified in any of these lists
will use the heuristic.  E.g:

  (setq ws-trim-global-modes '(guess (Info-mode) (not c-mode c++-mode)))

turns on WS Trim in Info-mode (God knows why), off in C mode and
C++ mode, and uses the heuristic for all other modes.")

(autoload 'global-ws-trim-mode "ws-trim" "\
Toggle Global WS Trim mode.
A positive prefix argument turns the mode on, any other prefix turns
it off.

When this mode is active, WS Trim mode is automagically turned on or
off in buffers depending on their major modes.  The behavior is
controlled by the `ws-trim-global-modes' variable.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (yas/minor-mode yas/snippet-dirs) "yasnippet" "yasnippet/yasnippet.el"
;;;;;;  (19824 54050))
;;; Generated autoloads from yasnippet/yasnippet.el

(defvar yas/snippet-dirs nil "\
Directory or list of snippet dirs for each major mode.

The directory where user-created snippets are to be stored. Can
also be a list of directories. In that case, when used for
bulk (re)loading of snippets (at startup or via
`yas/reload-all'), directories appearing earlier in the list
shadow other dir's snippets. Also, the first directory is taken
as the default for storing the user's new snippets.")

(custom-autoload 'yas/snippet-dirs "yasnippet" nil)

(autoload 'yas/minor-mode "yasnippet" "\
Toggle YASnippet mode.

When YASnippet mode is enabled, the `tas/trigger-key' key expands
snippets of code depending on the mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

You can customize the key through `yas/trigger-key'.

Key bindings:
\\{yas/minor-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("Pymacs/pymacs.el" "anything-extension.el"
;;;;;;  "anything-match-plugin.el" "auto-async-byte-compile.el" "auto-capitalize.el"
;;;;;;  "auto-complete-config.el" "auto-complete-extension.el" "auto-complete.el"
;;;;;;  "autopair.el" "backpack.el" "basic-edit-toolkit.el" "bbdb-vcard-import.el"
;;;;;;  "bbdb/bits/bbdb-filters/bbdb-ccmail.el" "bbdb/bits/bbdb-filters/bbdb-eudora.el"
;;;;;;  "bbdb/bits/bbdb-filters/bbdb-export.el" "bbdb/bits/bbdb-filters/bbdb-hp200lx.el"
;;;;;;  "bbdb/bits/bbdb-filters/bbdb-passwd.el" "bbdb/bits/bbdb-filters/bbdb-ph.el"
;;;;;;  "bbdb/bits/bbdb-ldif.el" "bbdb/bits/bbdb-mail-folders.el"
;;;;;;  "bbdb/bits/bbdb-signature.el" "bbdb/bits/bbdb-sort-mailrc.el"
;;;;;;  "bbdb/bits/bbdb-to-outlook.el" "bbdb/lisp/bbdb-autoloads.el"
;;;;;;  "bbdb/loadpath.el" "bbdb/misc/bbdb-unmigrate-stuff.el" "bigclock.el"
;;;;;;  "bitlbee.el" "bookmark-plus/bookmark+-chg.el" "bookmark-plus/bookmark+-doc.el"
;;;;;;  "bubble-buffer.el" "buffer-move.el" "byte-code-cache.el"
;;;;;;  "bzr.el" "c-sig.el" "calendar-howm.el" "cedet/cedet-build.el"
;;;;;;  "cedet/cedet-ediff.el" "cedet/cedet-update-changelog.el"
;;;;;;  "cedet/cedet-update-version.el" "cedet/cogre/cogre-load.el"
;;;;;;  "cedet/cogre/cogre-loaddefs.el" "cedet/cogre/wisent-dot-wy.el"
;;;;;;  "cedet/common/cedet-load.el" "cedet/common/cedet-loaddefs.el"
;;;;;;  "cedet/common/cedet.el" "cedet/common/ezimage.el" "cedet/common/working.el"
;;;;;;  "cedet/contrib/cedet-contrib-load.el" "cedet/contrib/cedet-contrib.el"
;;;;;;  "cedet/contrib/contrib-loaddefs.el" "cedet/contrib/semantic-ectag-scala.el"
;;;;;;  "cedet/contrib/wisent-csharp-wy.el" "cedet/contrib/wisent-php-wy.el"
;;;;;;  "cedet/contrib/wisent-ruby-wy.el" "cedet/contrib/wisent-ruby.el"
;;;;;;  "cedet/ede/autoconf-compat.el" "cedet/ede/ede-dired.el" "cedet/ede/ede-load.el"
;;;;;;  "cedet/ede/ede-loaddefs.el" "cedet/ede/ede-pconf.el" "cedet/ede/ede-proj-archive.el"
;;;;;;  "cedet/ede/ede-proj-aux.el" "cedet/ede/ede-proj-comp.el"
;;;;;;  "cedet/ede/ede-proj-elisp.el" "cedet/ede/ede-proj-info.el"
;;;;;;  "cedet/ede/ede-proj-maven2.el" "cedet/ede/ede-proj-misc.el"
;;;;;;  "cedet/ede/ede-proj-obj.el" "cedet/ede/ede-proj-prog.el"
;;;;;;  "cedet/ede/ede-proj-scheme.el" "cedet/ede/ede-proj-shared.el"
;;;;;;  "cedet/ede/ede-proj-skel.el" "cedet/ede/ede-source.el" "cedet/ede/ede-speedbar.el"
;;;;;;  "cedet/ede/ede-system.el" "cedet/ede/makefile-edit.el" "cedet/ede/project-am.el"
;;;;;;  "cedet/eieio/eieio-base.el" "cedet/eieio/eieio-comp.el" "cedet/eieio/eieio-custom.el"
;;;;;;  "cedet/eieio/eieio-doc.el" "cedet/eieio/eieio-load.el" "cedet/eieio/eieio-loaddefs.el"
;;;;;;  "cedet/eieio/eieio-speedbar.el" "cedet/eieio/eieio-test-methodinvoke.el"
;;;;;;  "cedet/eieio/eieio-test-mro.el" "cedet/eieio/eieio-tests.el"
;;;;;;  "cedet/eieio/eieio.el" "cedet/eieio/eieiocomp.el" "cedet/eieio/psql.el"
;;;;;;  "cedet/quickpeek/qp-base.el" "cedet/quickpeek/qp-c.el" "cedet/quickpeek/qp-elisp.el"
;;;;;;  "cedet/quickpeek/qp-util.el" "cedet/semantic/bovine/bovine-grammar-macros.el"
;;;;;;  "cedet/semantic/bovine/erlang-edoc.el" "cedet/semantic/bovine/semantic-c-by.el"
;;;;;;  "cedet/semantic/bovine/semantic-erlang-by.el" "cedet/semantic/bovine/semantic-erlang.el"
;;;;;;  "cedet/semantic/bovine/semantic-java.el" "cedet/semantic/bovine/semantic-make-by.el"
;;;;;;  "cedet/semantic/bovine/semantic-scm-by.el" "cedet/semantic/bovine/semantic-skeleton-by.el"
;;;;;;  "cedet/semantic/ctags/semantic-ectag-util.el" "cedet/semantic/semantic-analyze-fcn.el"
;;;;;;  "cedet/semantic/semantic-ast.el" "cedet/semantic/semantic-example.el"
;;;;;;  "cedet/semantic/semantic-fw.el" "cedet/semantic/semantic-grammar-wy.el"
;;;;;;  "cedet/semantic/semantic-inc.el" "cedet/semantic/semantic-loaddefs.el"
;;;;;;  "cedet/semantic/semantic-sb.el" "cedet/semantic/semantic-util.el"
;;;;;;  "cedet/semantic/semanticdb-el.el" "cedet/semantic/semanticdb-java.el"
;;;;;;  "cedet/semantic/semanticdb-javascript.el" "cedet/semantic/semanticdb-mk.el"
;;;;;;  "cedet/semantic/semanticdb-skel.el" "cedet/semantic/semanticdb-system.el"
;;;;;;  "cedet/semantic/wisent/semantic-wisent.el" "cedet/semantic/wisent/wisent-awk-wy.el"
;;;;;;  "cedet/semantic/wisent/wisent-calc-wy.el" "cedet/semantic/wisent/wisent-cim-wy.el"
;;;;;;  "cedet/semantic/wisent/wisent-expr.el" "cedet/semantic/wisent/wisent-grammar-macros.el"
;;;;;;  "cedet/semantic/wisent/wisent-java-tags-wy.el" "cedet/semantic/wisent/wisent-java-wy.el"
;;;;;;  "cedet/semantic/wisent/wisent-java.el" "cedet/semantic/wisent/wisent-javascript-jv-wy.el"
;;;;;;  "cedet/semantic/wisent/wisent-python-wy.el" "cedet/speedbar/bigclock.el"
;;;;;;  "cedet/speedbar/dframe.el" "cedet/speedbar/fsum.el" "cedet/speedbar/sb-ant.el"
;;;;;;  "cedet/speedbar/sb-html.el" "cedet/speedbar/sb-image.el"
;;;;;;  "cedet/speedbar/sb-texinfo.el" "cedet/speedbar/speedbar-load.el"
;;;;;;  "cedet/speedbar/speedbar-loaddefs.el" "cedet/srecode/srecode-document-vars.el"
;;;;;;  "cedet/srecode/srecode-extract.el" "cedet/srecode/srecode-load.el"
;;;;;;  "cedet/srecode/srecode-loaddefs.el" "cedet/srecode/srecode-template-wy.el"
;;;;;;  "cedet/srecode/srecode.el" "cedet/tests/cedet-integ-test.el"
;;;;;;  "cedet/tests/cit-cpp.el" "cedet/tests/cit-dist.el" "cedet/tests/cit-el.el"
;;;;;;  "cedet/tests/cit-externaldb.el" "cedet/tests/cit-gnustep.el"
;;;;;;  "cedet/tests/cit-load.el" "cedet/tests/cit-srec.el" "cedet/tests/cit-symref.el"
;;;;;;  "cedet/tests/cit-texi.el" "cedet/tests/cit-uml.el" "ch6-bbdb-import-csv-buffer.el"
;;;;;;  "chop.el" "circe/circe-auto.el" "circe/circe-chanop.el" "circe/circe-e21.el"
;;;;;;  "circe/circe-xemacs.el" "circe/incomplete.el" "circe/lcs.el"
;;;;;;  "circe/lui-format.el" "circe/lui-logging.el" "circe/lui-xemacs.el"
;;;;;;  "circe/lui.el" "circe/scripts/build-helper.el" "circe/tracking.el"
;;;;;;  "color-moccur.el" "color-theme-6.6.0/color-theme-autoloads.el"
;;;;;;  "color-theme-6.6.0/themes/color-theme-example.el" "color-theme-6.6.0/themes/color-theme-library.el"
;;;;;;  "command-frequency.el" "compile-utils.el" "csv.el" "dar.el"
;;;;;;  "dired-details.el" "dired-extension.el" "django-mode.el"
;;;;;;  "electric-dot-and-dash.el" "elscreen-color-theme.el" "elscreen-dired.el"
;;;;;;  "elscreen-howm.el" "elscreen-server.el" "elscreen-speedbar.el"
;;;;;;  "elscreen-w3m.el" "elscreen-wl.el" "elscreen.el" "emacs-w3m/mew-w3m.el"
;;;;;;  "emacs-w3m/shimbun/sb-2ch.el" "emacs-w3m/shimbun/sb-airs.el"
;;;;;;  "emacs-w3m/shimbun/sb-aljazeera.el" "emacs-w3m/shimbun/sb-arch-bluegate.el"
;;;;;;  "emacs-w3m/shimbun/sb-asahi-html.el" "emacs-w3m/shimbun/sb-asahi-mytown.el"
;;;;;;  "emacs-w3m/shimbun/sb-asahi.el" "emacs-w3m/shimbun/sb-atmarkit.el"
;;;;;;  "emacs-w3m/shimbun/sb-atom-hash.el" "emacs-w3m/shimbun/sb-atom.el"
;;;;;;  "emacs-w3m/shimbun/sb-bbc.el" "emacs-w3m/shimbun/sb-bbdb-ml.el"
;;;;;;  "emacs-w3m/shimbun/sb-cgi-board.el" "emacs-w3m/shimbun/sb-cnet-jp.el"
;;;;;;  "emacs-w3m/shimbun/sb-cnet.el" "emacs-w3m/shimbun/sb-cnn-jp.el"
;;;;;;  "emacs-w3m/shimbun/sb-coldsync.el" "emacs-w3m/shimbun/sb-debian-jp.el"
;;;;;;  "emacs-w3m/shimbun/sb-debian.el" "emacs-w3m/shimbun/sb-debugmagazin-de.el"
;;;;;;  "emacs-w3m/shimbun/sb-dennou.el" "emacs-w3m/shimbun/sb-digiko.el"
;;;;;;  "emacs-w3m/shimbun/sb-elips.el" "emacs-w3m/shimbun/sb-emacs-w3m.el"
;;;;;;  "emacs-w3m/shimbun/sb-emacswiki.el" "emacs-w3m/shimbun/sb-engadget-ja.el"
;;;;;;  "emacs-w3m/shimbun/sb-excite.el" "emacs-w3m/shimbun/sb-exconn.el"
;;;;;;  "emacs-w3m/shimbun/sb-f1fan.el" "emacs-w3m/shimbun/sb-fau.el"
;;;;;;  "emacs-w3m/shimbun/sb-ffii.el" "emacs-w3m/shimbun/sb-fml.el"
;;;;;;  "emacs-w3m/shimbun/sb-gendai-net.el" "emacs-w3m/shimbun/sb-geocrawler.el"
;;;;;;  "emacs-w3m/shimbun/sb-glimpse.el" "emacs-w3m/shimbun/sb-gnome.el"
;;;;;;  "emacs-w3m/shimbun/sb-haiku-os.el" "emacs-w3m/shimbun/sb-hash.el"
;;;;;;  "emacs-w3m/shimbun/sb-heise.el" "emacs-w3m/shimbun/sb-hns.el"
;;;;;;  "emacs-w3m/shimbun/sb-ibm-dev.el" "emacs-w3m/shimbun/sb-impress.el"
;;;;;;  "emacs-w3m/shimbun/sb-infoshop.el" "emacs-w3m/shimbun/sb-itmedia.el"
;;;;;;  "emacs-w3m/shimbun/sb-japantimes.el" "emacs-w3m/shimbun/sb-javaconf.el"
;;;;;;  "emacs-w3m/shimbun/sb-jpilot.el" "emacs-w3m/shimbun/sb-jpo.el"
;;;;;;  "emacs-w3m/shimbun/sb-kantei.el" "emacs-w3m/shimbun/sb-kde.el"
;;;;;;  "emacs-w3m/shimbun/sb-laut-de.el" "emacs-w3m/shimbun/sb-linuxce-jp.el"
;;;;;;  "emacs-w3m/shimbun/sb-lotusex.el" "emacs-w3m/shimbun/sb-lump.el"
;;;;;;  "emacs-w3m/shimbun/sb-m17n.el" "emacs-w3m/shimbun/sb-macosx-jp.el"
;;;;;;  "emacs-w3m/shimbun/sb-mailarc.el" "emacs-w3m/shimbun/sb-mailman.el"
;;;;;;  "emacs-w3m/shimbun/sb-mainichi.el" "emacs-w3m/shimbun/sb-makanai.el"
;;;;;;  "emacs-w3m/shimbun/sb-marc-aims.el" "emacs-w3m/shimbun/sb-meadow.el"
;;;;;;  "emacs-w3m/shimbun/sb-mew.el" "emacs-w3m/shimbun/sb-mhonarc.el"
;;;;;;  "emacs-w3m/shimbun/sb-mozilla-jp.el" "emacs-w3m/shimbun/sb-msdn.el"
;;;;;;  "emacs-w3m/shimbun/sb-msports-nifty.el" "emacs-w3m/shimbun/sb-multi.el"
;;;;;;  "emacs-w3m/shimbun/sb-n24-de.el" "emacs-w3m/shimbun/sb-namazu.el"
;;;;;;  "emacs-w3m/shimbun/sb-netbsd.el" "emacs-w3m/shimbun/sb-nikkansports.el"
;;;;;;  "emacs-w3m/shimbun/sb-nikkei.el" "emacs-w3m/shimbun/sb-nytimes.el"
;;;;;;  "emacs-w3m/shimbun/sb-opentechpress-jp.el" "emacs-w3m/shimbun/sb-orca.el"
;;;;;;  "emacs-w3m/shimbun/sb-palmfan.el" "emacs-w3m/shimbun/sb-pcweb-column.el"
;;;;;;  "emacs-w3m/shimbun/sb-perlentaucher-de.el" "emacs-w3m/shimbun/sb-pilot-link.el"
;;;;;;  "emacs-w3m/shimbun/sb-pilot-mailsync.el" "emacs-w3m/shimbun/sb-plucker.el"
;;;;;;  "emacs-w3m/shimbun/sb-pocketgames.el" "emacs-w3m/shimbun/sb-rakuten.el"
;;;;;;  "emacs-w3m/shimbun/sb-redhat.el" "emacs-w3m/shimbun/sb-rediff.el"
;;;;;;  "emacs-w3m/shimbun/sb-rss-blogs.el" "emacs-w3m/shimbun/sb-rss-hash.el"
;;;;;;  "emacs-w3m/shimbun/sb-rss.el" "emacs-w3m/shimbun/sb-ruby.el"
;;;;;;  "emacs-w3m/shimbun/sb-sankei.el" "emacs-w3m/shimbun/sb-savannah.el"
;;;;;;  "emacs-w3m/shimbun/sb-scipy.el" "emacs-w3m/shimbun/sb-security-memo.el"
;;;;;;  "emacs-w3m/shimbun/sb-slashdot-jp.el" "emacs-w3m/shimbun/sb-slashdot.el"
;;;;;;  "emacs-w3m/shimbun/sb-sml.el" "emacs-w3m/shimbun/sb-sourceforge-jp.el"
;;;;;;  "emacs-w3m/shimbun/sb-spiegel.el" "emacs-w3m/shimbun/sb-sponichi.el"
;;;;;;  "emacs-w3m/shimbun/sb-squeak-dev.el" "emacs-w3m/shimbun/sb-squeak-ja.el"
;;;;;;  "emacs-w3m/shimbun/sb-sueddeutsche-de.el" "emacs-w3m/shimbun/sb-tcup.el"
;;;;;;  "emacs-w3m/shimbun/sb-tdiary-ml.el" "emacs-w3m/shimbun/sb-tdiary.el"
;;;;;;  "emacs-w3m/shimbun/sb-tech-on.el" "emacs-w3m/shimbun/sb-texfaq.el"
;;;;;;  "emacs-w3m/shimbun/sb-text.el" "emacs-w3m/shimbun/sb-the-onion.el"
;;;;;;  "emacs-w3m/shimbun/sb-the-register.el" "emacs-w3m/shimbun/sb-tigris.el"
;;;;;;  "emacs-w3m/shimbun/sb-toshiba.el" "emacs-w3m/shimbun/sb-vinelinux.el"
;;;;;;  "emacs-w3m/shimbun/sb-w3m-dev.el" "emacs-w3m/shimbun/sb-welt-de.el"
;;;;;;  "emacs-w3m/shimbun/sb-wiki.el" "emacs-w3m/shimbun/sb-wikimedia.el"
;;;;;;  "emacs-w3m/shimbun/sb-wincefan.el" "emacs-w3m/shimbun/sb-wired-jp.el"
;;;;;;  "emacs-w3m/shimbun/sb-x51.el" "emacs-w3m/shimbun/sb-xemacs.el"
;;;;;;  "emacs-w3m/shimbun/sb-yahoo-auctions.el" "emacs-w3m/shimbun/sb-yahoo-sports.el"
;;;;;;  "emacs-w3m/shimbun/sb-yahoo.el" "emacs-w3m/shimbun/sb-yomiuri-html.el"
;;;;;;  "emacs-w3m/shimbun/sb-yomiuri.el" "emacs-w3m/shimbun/sb-zdnet-jp.el"
;;;;;;  "emacs-w3m/shimbun/sb-zeit-de.el" "emacs-w3m/shimbun/shimbun.el"
;;;;;;  "emacs-w3m/w3m-bug.el" "emacs-w3m/w3m-ccl.el" "emacs-w3m/w3m-ems.el"
;;;;;;  "emacs-w3m/w3m-favicon.el" "emacs-w3m/w3m-fb.el" "emacs-w3m/w3m-hist.el"
;;;;;;  "emacs-w3m/w3m-image.el" "emacs-w3m/w3m-load.el" "emacs-w3m/w3m-mail.el"
;;;;;;  "emacs-w3m/w3m-proc.el" "emacs-w3m/w3m-rss.el" "emacs-w3m/w3m-tabmenu.el"
;;;;;;  "emacs-w3m/w3m-ucs.el" "emacs-w3m/w3m-util.el" "emacs-w3m/w3m-xmas.el"
;;;;;;  "emacs-w3m/w3mhack.el" "emms/lisp/emms-auto.el" "emms/lisp/emms-bookmarks.el"
;;;;;;  "emms/lisp/emms-browser.el" "emms/lisp/emms-compat.el" "emms/lisp/emms-cue.el"
;;;;;;  "emms/lisp/emms-history.el" "emms/lisp/emms-i18n.el" "emms/lisp/emms-info-libtag.el"
;;;;;;  "emms/lisp/emms-info-metaflac.el" "emms/lisp/emms-info-mp3info.el"
;;;;;;  "emms/lisp/emms-info-ogginfo.el" "emms/lisp/emms-info.el"
;;;;;;  "emms/lisp/emms-last-played.el" "emms/lisp/emms-lastfm-client.el"
;;;;;;  "emms/lisp/emms-lastfm-scrobbler.el" "emms/lisp/emms-maint.el"
;;;;;;  "emms/lisp/emms-mark.el" "emms/lisp/emms-metaplaylist-mode.el"
;;;;;;  "emms/lisp/emms-mode-line-icon.el" "emms/lisp/emms-player-mpg321-remote.el"
;;;;;;  "emms/lisp/emms-player-mplayer.el" "emms/lisp/emms-player-simple.el"
;;;;;;  "emms/lisp/emms-player-vlc.el" "emms/lisp/emms-player-xine.el"
;;;;;;  "emms/lisp/emms-playlist-sort.el" "emms/lisp/emms-stream-info.el"
;;;;;;  "emms/lisp/emms-tag-editor.el" "emms/lisp/emms-url.el" "emms/lisp/emms.el"
;;;;;;  "emms/lisp/jack.el" "emms/lisp/later-do.el" "escreen.el"
;;;;;;  "filladapt.el" "fuzzy.el" "gst-mode.el" "hexview-mode.el"
;;;;;;  "highrise.el" "html-php.el" "html-script.el" "ion3.el" "jira.el"
;;;;;;  "joseph-autopair.el" "json.el" "lazycat-toolkit.el" "lookout.el"
;;;;;;  "mailcrypt-3.5.9/mc-remail2.el" "markdown-mode/markdown-mode.el"
;;;;;;  "maxframe.el" "mhc/emacs/mhc-bm.el" "mhc/emacs/mhc-calendar.el"
;;;;;;  "mhc/emacs/mhc-compat.el" "mhc/emacs/mhc-cvs.el" "mhc/emacs/mhc-date.el"
;;;;;;  "mhc/emacs/mhc-day.el" "mhc/emacs/mhc-db.el" "mhc/emacs/mhc-draft.el"
;;;;;;  "mhc/emacs/mhc-e21.el" "mhc/emacs/mhc-face.el" "mhc/emacs/mhc-file.el"
;;;;;;  "mhc/emacs/mhc-guess.el" "mhc/emacs/mhc-header.el" "mhc/emacs/mhc-logic.el"
;;;;;;  "mhc/emacs/mhc-mime.el" "mhc/emacs/mhc-minibuf.el" "mhc/emacs/mhc-misc.el"
;;;;;;  "mhc/emacs/mhc-parse.el" "mhc/emacs/mhc-record.el" "mhc/emacs/mhc-schedule.el"
;;;;;;  "mhc/emacs/mhc-slot.el" "mhc/emacs/mhc-summary.el" "mhc/emacs/mhc-sync.el"
;;;;;;  "mhc/emacs/mhc-vars.el" "mhc/emacs/mhc-xmas.el" "mhc/emacs/mhc.el"
;;;;;;  "mhc/emacs/nnmhc.el" "mouse-embrace.el" "mpg123.el" "newsticker-1.99/newsticker-testsuite.el"
;;;;;;  "newsticker-1.99/newsticker.el" "newsticker-extension.el"
;;;;;;  "newsticker-notify.el" "oz/mozart.el" "oz/oz-extra.el" "oz/oz-server.el"
;;;;;;  "oz/oz.el" "pabbrev.el" "pager.el" "pair-mode.el" "paredit-extension.el"
;;;;;;  "paredit-viper-compat.el" "parenface.el" "popup.el" "pos-tip.el"
;;;;;;  "predictive/auto-overlay-common.el" "predictive/auto-overlay-flat.el"
;;;;;;  "predictive/auto-overlay-line.el" "predictive/auto-overlay-nested.el"
;;;;;;  "predictive/auto-overlay-self.el" "predictive/auto-overlay-word.el"
;;;;;;  "predictive/auto-overlays-compat.el" "predictive/auto-overlays.el"
;;;;;;  "predictive/avl-tree.el" "predictive/completion-ui-dynamic.el"
;;;;;;  "predictive/completion-ui-echo.el" "predictive/completion-ui-hotkeys.el"
;;;;;;  "predictive/completion-ui-menu.el" "predictive/completion-ui-popup-frame.el"
;;;;;;  "predictive/completion-ui-sources.el" "predictive/completion-ui-tooltip.el"
;;;;;;  "predictive/completion-ui.el" "predictive/dict-english.el"
;;;;;;  "predictive/dict-tree.el" "predictive/heap.el" "predictive/pos-tip.el"
;;;;;;  "predictive/predictive-auto-overlay-auto-dict.el" "predictive/predictive-compat.el"
;;;;;;  "predictive/predictive-convert-dump-format.el" "predictive/predictive-html.el"
;;;;;;  "predictive/predictive-latex-graphicx.el" "predictive/predictive-latex.el"
;;;;;;  "predictive/predictive-texinfo.el" "predictive/predictive.el"
;;;;;;  "predictive/queue.el" "predictive/tNFA.el" "predictive/trie.el"
;;;;;;  "progr-align.el" "python-mode/pycomplete.el" "rect-extension.el"
;;;;;;  "redo+.el" "redo.el" "regex-tool.el" "remember/read-file-name.el"
;;;;;;  "remember/remember-autoloads.el" "remember/remember-experimental.el"
;;;;;;  "remember/scripts/remember-build.el" "rest-api.el" "revive+.el"
;;;;;;  "rinari/util/jump/findr.el" "rinari/util/ruby-compilation-rspec.el"
;;;;;;  "shell-command-extension.el" "simple-call-tree.el" "smalltalk-mode-init.el"
;;;;;;  "smalltalk-mode.el" "subdirs.el" "sunrise-commander.el" "textmate-mode.el"
;;;;;;  "textmate.el" "tiling.el" "typing-speed.el" "unbound.el"
;;;;;;  "vimpulse/test-framework.el" "vimpulse/vimpulse-compatibility.el"
;;;;;;  "vimpulse/vimpulse-dependencies.el" "vimpulse/vimpulse-ex.el"
;;;;;;  "vimpulse/vimpulse-misc-keybindings.el" "vimpulse/vimpulse-modal.el"
;;;;;;  "vimpulse/vimpulse-operator.el" "vimpulse/vimpulse-paren-matching.el"
;;;;;;  "vimpulse/vimpulse-test.el" "vimpulse/vimpulse-text-object-system.el"
;;;;;;  "vimpulse/vimpulse-utils.el" "vimpulse/vimpulse-viper-function-redefinitions.el"
;;;;;;  "vimpulse/vimpulse-visual-mode.el" "vimpulse/vimpulse.el"
;;;;;;  "viper-in-more-modes.el" "w3m-lnum.el" "wdired-extension.el"
;;;;;;  "weblogger.el" "widen-window.el" "window-number.el" "wl-highlight-ad.el"
;;;;;;  "woof.el" "x-dict.el" "xml-rpc.el" "xte.el" "xwindow-ring.el"
;;;;;;  "yaml-mode.el" "yas-jit.el" "yasnippet/dropdown-list.el"
;;;;;;  "yasnippet/yasnippet-debug.el") (19833 13262 265048))

;;;***

;;;### (autoloads (anything-c-reset-adaptative-history anything-c-set-variable
;;;;;;  anything-c-call-interactively w32-shell-execute-open-file
;;;;;;  anything-ratpoison-commands anything-c-run-external-command
;;;;;;  anything-c-shell-command-if-needed anything-apt anything-world-time
;;;;;;  anything-select-xfont anything-top anything-create anything-execute-anything-command
;;;;;;  anything-call-source anything-surfraw anything-calcul-expression
;;;;;;  anything-eval-expression-with-eldoc anything-eval-expression
;;;;;;  anything-yaoddmuse-emacswiki-post-library anything-yaoddmuse-emacswiki-edit-or-view
;;;;;;  anything-yaoddmuse-cache-pages anything-all-mark-rings anything-global-mark-ring
;;;;;;  anything-mark-ring anything-simple-call-tree anything-bookmark-ext
;;;;;;  anything-manage-advice anything-M-x anything-filelist+ anything-filelist
;;;;;;  anything-yank-text-at-point anything-c-goto-next-file anything-c-goto-precedent-file
;;;;;;  anything-do-grep anything-dired-bindings anything-dired-hardlink-file
;;;;;;  anything-dired-symlink-file anything-dired-copy-file anything-dired-rename-file
;;;;;;  anything-insert-file anything-write-file anything-find-files
;;;;;;  anything-regexp anything-kill-buffers anything-org-headlines
;;;;;;  anything-browse-code anything-occur anything-list-emacs-process
;;;;;;  anything-timers anything-bm-list anything-eev-anchors anything-emms
;;;;;;  anything-org-keywords anything-man-woman anything-register
;;;;;;  anything-c-insert-latex-math anything-c-pp-bookmarks anything-bookmarks
;;;;;;  anything-colors anything-firefox-bookmarks anything-w3m-bookmarks
;;;;;;  anything-locate anything-bbdb anything-buffers+ anything-for-buffers
;;;;;;  anything-yahoo-suggest anything-google-suggest anything-imenu
;;;;;;  anything-gentoo anything-minibuffer-history anything-show-kill-ring
;;;;;;  anything-info-emacs anything-info-at-point anything-recentf
;;;;;;  anything-for-files anything-mini anything-configuration)
;;;;;;  "anything-config" "anything-config.el" (19826 32852))
;;; Generated autoloads from anything-config.el

(autoload 'anything-configuration "anything-config" "\
Customize `anything'.

\(fn)" t nil)

(defvar anything-command-map)

(autoload 'anything-mini "anything-config" "\
Preconfigured `anything' lightweight version (buffer -> recentf).

\(fn)" t nil)

(autoload 'anything-for-files "anything-config" "\
Preconfigured `anything' for opening files.
ffap -> recentf -> buffer -> bookmark -> file-cache -> files-in-current-dir -> locate

\(fn)" t nil)

(autoload 'anything-recentf "anything-config" "\
Preconfigured `anything' for `recentf'.

\(fn)" t nil)

(autoload 'anything-info-at-point "anything-config" "\
Preconfigured `anything' for searching info at point.

\(fn)" t nil)

(autoload 'anything-info-emacs "anything-config" "\
Preconfigured anything for Emacs manual index.

\(fn)" t nil)

(autoload 'anything-show-kill-ring "anything-config" "\
Preconfigured `anything' for `kill-ring'. It is drop-in replacement of `yank-pop'.
You may bind this command to M-y.

\(fn)" t nil)

(autoload 'anything-minibuffer-history "anything-config" "\
Preconfigured `anything' for `minibuffer-history'.

\(fn)" t nil)

(autoload 'anything-gentoo "anything-config" "\
Preconfigured `anything' for gentoo linux.

\(fn)" t nil)

(autoload 'anything-imenu "anything-config" "\
Preconfigured `anything' for `imenu'.

\(fn)" t nil)

(autoload 'anything-google-suggest "anything-config" "\
Preconfigured `anything' for google search with google suggest.

\(fn)" t nil)

(autoload 'anything-yahoo-suggest "anything-config" "\
Preconfigured `anything' for Yahoo searching with Yahoo suggest.

\(fn)" t nil)

(autoload 'anything-for-buffers "anything-config" "\
Preconfigured `anything' for buffer.

\(fn)" t nil)

(autoload 'anything-buffers+ "anything-config" "\
Enhanced preconfigured `anything' for buffer.

\(fn)" t nil)

(autoload 'anything-bbdb "anything-config" "\
Preconfigured `anything' for BBDB.

Needs BBDB.

http://bbdb.sourceforge.net/

\(fn)" t nil)

(autoload 'anything-locate "anything-config" "\
Preconfigured `anything' for Locate.
Note you can add locate command after entering pattern.
See man locate for more infos.

\(fn)" t nil)

(autoload 'anything-w3m-bookmarks "anything-config" "\
Preconfigured `anything' for w3m bookmark.

Needs w3m and emacs-w3m.

http://w3m.sourceforge.net/
http://emacs-w3m.namazu.org/

\(fn)" t nil)

(autoload 'anything-firefox-bookmarks "anything-config" "\
Preconfigured `anything' for firefox bookmark.
You will have to enable html bookmarks in firefox:
open about:config in firefox and double click on this line to enable value to true:

user_pref(\"browser.bookmarks.autoExportHTML\", false);

You should have now:

user_pref(\"browser.bookmarks.autoExportHTML\", true);

After closing firefox, you will be able to browse you bookmarks.

\(fn)" t nil)

(autoload 'anything-colors "anything-config" "\
Preconfigured `anything' for color.

\(fn)" t nil)

(autoload 'anything-bookmarks "anything-config" "\
Preconfigured `anything' for bookmarks.

\(fn)" t nil)

(autoload 'anything-c-pp-bookmarks "anything-config" "\
Preconfigured `anything' for bookmarks (pretty-printed).

\(fn)" t nil)

(autoload 'anything-c-insert-latex-math "anything-config" "\
Preconfigured anything for latex math symbols completion.

\(fn)" t nil)

(autoload 'anything-register "anything-config" "\
Preconfigured `anything' for Emacs registers.

\(fn)" t nil)

(autoload 'anything-man-woman "anything-config" "\
Preconfigured `anything' for Man and Woman pages.

\(fn)" t nil)

(autoload 'anything-org-keywords "anything-config" "\
Preconfigured `anything' for org keywords.

\(fn)" t nil)

(autoload 'anything-emms "anything-config" "\
Preconfigured `anything' for emms sources.

\(fn)" t nil)

(autoload 'anything-eev-anchors "anything-config" "\
Preconfigured `anything' for eev anchors.

\(fn)" t nil)

(autoload 'anything-bm-list "anything-config" "\
Preconfigured `anything' for visible bookmarks.

Needs bm.el

http://cvs.savannah.gnu.org/viewvc/*checkout*/bm/bm/bm.el

\(fn)" t nil)

(autoload 'anything-timers "anything-config" "\
Preconfigured `anything' for timers.

\(fn)" t nil)

(autoload 'anything-list-emacs-process "anything-config" "\
Preconfigured `anything' for emacs process.

\(fn)" t nil)

(autoload 'anything-occur "anything-config" "\
Preconfigured Anything for Occur source.

\(fn)" t nil)

(autoload 'anything-browse-code "anything-config" "\
Preconfigured anything to browse code.

\(fn)" t nil)

(autoload 'anything-org-headlines "anything-config" "\
Preconfigured anything to show org headlines.

\(fn)" t nil)

(autoload 'anything-kill-buffers "anything-config" "\
Preconfigured `anything' to kill buffer you selected.

\(fn)" t nil)

(autoload 'anything-regexp "anything-config" "\
Preconfigured anything to build regexps and run query-replace-regexp against.

\(fn)" t nil)

(autoload 'anything-find-files "anything-config" "\
Preconfigured `anything' for anything implementation of `find-file'.
Called with a prefix arg show history if some.
Don't call it from programs, use `anything-find-files1' instead.
This is the starting point for nearly all actions you can do on files.

\(fn)" t nil)

(autoload 'anything-write-file "anything-config" "\
Preconfigured `anything' providing completion for `write-file'.

\(fn)" t nil)

(autoload 'anything-insert-file "anything-config" "\
Preconfigured `anything' providing completion for `insert-file'.

\(fn)" t nil)

(autoload 'anything-dired-rename-file "anything-config" "\
Preconfigured `anything' to rename files from dired.

\(fn)" t nil)

(autoload 'anything-dired-copy-file "anything-config" "\
Preconfigured `anything' to copy files from dired.

\(fn)" t nil)

(autoload 'anything-dired-symlink-file "anything-config" "\
Preconfigured `anything' to symlink files from dired.

\(fn)" t nil)

(autoload 'anything-dired-hardlink-file "anything-config" "\
Preconfigured `anything' to hardlink files from dired.

\(fn)" t nil)

(autoload 'anything-dired-bindings "anything-config" "\
Replace usual dired commands `C' and `R' by anything ones.
When call interactively toggle dired bindings and anything bindings.
When call non--interactively with arg > 0, enable anything bindings.
You can put (anything-dired-binding 1) in init file to enable anything bindings.

\(fn &optional ARG)" t nil)

(autoload 'anything-do-grep "anything-config" "\
Preconfigured anything for grep.
Contrarily to Emacs `grep' no default directory is given, but
the full path of candidates in ONLY.
That allow to grep different files not only in `default-directory' but anywhere
by marking them (C-<SPACE>). If one or more directory is selected
grep will search in all files of these directories.
You can use also wildcard in the base name of candidate.
If a prefix arg is given use the -r option of grep.
See also `anything-do-grep1'.

\(fn)" t nil)

(autoload 'anything-c-goto-precedent-file "anything-config" "\
Go to precedent file in anything grep/etags buffers.

\(fn)" t nil)

(autoload 'anything-c-goto-next-file "anything-config" "\
Go to precedent file in anything grep/etags buffers.

\(fn)" t nil)

(autoload 'anything-yank-text-at-point "anything-config" "\
Yank text at point in minibuffer.

\(fn)" t nil)

(autoload 'anything-filelist "anything-config" "\
Preconfigured `anything' to open files instantly.

See `anything-c-filelist-file-name' docstring for usage.

\(fn)" t nil)

(autoload 'anything-filelist+ "anything-config" "\
Preconfigured `anything' to open files/buffers/bookmarks instantly.

This is a replacement for `anything-for-files'.
See `anything-c-filelist-file-name' docstring for usage.

\(fn)" t nil)

(autoload 'anything-M-x "anything-config" "\
Preconfigured `anything' for Emacs commands.
It is `anything' replacement of regular `M-x' `execute-extended-command'.

\(fn)" t nil)

(autoload 'anything-manage-advice "anything-config" "\
Preconfigured `anything' to disable/enable function advices.

\(fn)" t nil)

(autoload 'anything-bookmark-ext "anything-config" "\
Preconfigured `anything' for bookmark-extensions sources.
Needs bookmark-ext.el

http://mercurial.intuxication.org/hg/emacs-bookmark-extension

\(fn)" t nil)

(autoload 'anything-simple-call-tree "anything-config" "\
Preconfigured `anything' for simple-call-tree. List function relationships.

Needs simple-call-tree.el.
http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el

\(fn)" t nil)

(autoload 'anything-mark-ring "anything-config" "\
Preconfigured `anything' for `anything-c-source-mark-ring'.

\(fn)" t nil)

(autoload 'anything-global-mark-ring "anything-config" "\
Preconfigured `anything' for `anything-c-source-global-mark-ring'.

\(fn)" t nil)

(autoload 'anything-all-mark-rings "anything-config" "\
Preconfigured `anything' for `anything-c-source-global-mark-ring' and `anything-c-source-mark-ring'.

\(fn)" t nil)

(autoload 'anything-yaoddmuse-cache-pages "anything-config" "\
Fetch the list of files on emacswiki and create cache file.
If load is non--nil load the file and feed `yaoddmuse-pages-hash'.

\(fn &optional LOAD)" t nil)

(autoload 'anything-yaoddmuse-emacswiki-edit-or-view "anything-config" "\
Preconfigured `anything' to edit or view EmacsWiki page.

Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el

\(fn)" t nil)

(autoload 'anything-yaoddmuse-emacswiki-post-library "anything-config" "\
Preconfigured `anything' to post library to EmacsWiki.

Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el

\(fn)" t nil)

(autoload 'anything-eval-expression "anything-config" "\
Preconfigured anything for `anything-c-source-evaluation-result'.

\(fn ARG)" t nil)

(autoload 'anything-eval-expression-with-eldoc "anything-config" "\
Preconfigured anything for `anything-c-source-evaluation-result' with `eldoc' support. 

\(fn)" t nil)

(autoload 'anything-calcul-expression "anything-config" "\
Preconfigured anything for `anything-c-source-calculation-result'.

\(fn)" t nil)

(autoload 'anything-surfraw "anything-config" "\
Preconfigured `anything' to search PATTERN with search ENGINE.

\(fn PATTERN ENGINE)" t nil)

(autoload 'anything-call-source "anything-config" "\
Preconfigured `anything' to call anything source.

\(fn)" t nil)

(autoload 'anything-execute-anything-command "anything-config" "\
Preconfigured `anything' to execute preconfigured `anything'.

\(fn)" t nil)

(autoload 'anything-create "anything-config" "\
Preconfigured `anything' to do many create actions from STRING.
See also `anything-create--actions'.

\(fn &optional STRING INITIAL-INPUT)" t nil)

(autoload 'anything-top "anything-config" "\
Preconfigured `anything' for top command.

\(fn)" t nil)

(autoload 'anything-select-xfont "anything-config" "\
Preconfigured `anything' to select Xfont.

\(fn)" t nil)

(autoload 'anything-world-time "anything-config" "\
Preconfigured `anything' to show world time.

\(fn)" t nil)

(autoload 'anything-apt "anything-config" "\
Preconfigured `anything' : frontend of APT package manager.

\(fn QUERY)" t nil)

(autoload 'anything-c-shell-command-if-needed "anything-config" "\
Not documented

\(fn COMMAND)" t nil)

(autoload 'anything-c-run-external-command "anything-config" "\
Preconfigured `anything' to run External PROGRAM asyncronously from Emacs.
If program is already running exit with error.
You can set your own list of commands with
`anything-c-external-commands-list'.

\(fn PROGRAM)" t nil)

(autoload 'anything-ratpoison-commands "anything-config" "\
Preconfigured `anything' to execute ratpoison commands.

\(fn)" t nil)

(autoload 'w32-shell-execute-open-file "anything-config" "\
Not documented

\(fn FILE)" t nil)

(autoload 'anything-c-call-interactively "anything-config" "\
Execute CMD-OR-NAME as Emacs command.
It is added to `extended-command-history'.
`anything-current-prefix-arg' is used as the command's prefix argument.

\(fn CMD-OR-NAME)" nil nil)

(autoload 'anything-c-set-variable "anything-config" "\
Set value to VAR interactively.

\(fn VAR)" t nil)

(autoload 'anything-c-reset-adaptative-history "anything-config" "\
Delete all `anything-c-adaptive-history' and his file.
Useful when you have a old or corrupted `anything-c-adaptive-history-file'.

\(fn)" t nil)

;;;***
