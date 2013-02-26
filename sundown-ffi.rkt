#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define)

(define-ffi-definer define-sundown
  (ffi-lib "/Users/greg/src/sundown/libsundown.so.1"))

(define _size_t _uintptr)
(define _voidptr _intptr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; /* buffer.h */
;;

;; typedef enum {
;; 	BUF_OK = 0,
;; 	BUF_ENOMEM = -1,
;; } buferror_t;

(define bufenum
  (_enum '(BUF_OK = 0
           BUF_ENOMEM = -1)))

;; /* struct buf: character array buffer */
;; struct buf {
;; 	uint8_t *data;		/* actual character data */
;; 	size_t size;	/* size of the string */
;; 	size_t asize;	/* allocated size (0 = volatile buffer) */
;; 	size_t unit;	/* reallocation unit size (0 = read-only buffer) */
;; };

(define-cstruct _buf
  ([data (_ptr io _uint8)]
   [size _size_t]
   [asize _size_t]
   [unit _size_t]))

(define buf-ptr _buf-pointer)
(define const-buf-ptr _buf-pointer) ;; TO-DO: const, how??

;; /* CONST_BUF: global buffer from a string litteral */
;; #define BUF_STATIC(string) \
;; 	{ (uint8_t *)string, sizeof string -1, sizeof string, 0, 0 }

;; /* VOLATILE_BUF: macro for creating a volatile buffer on the stack */
;; #define BUF_VOLATILE(strname) \
;; 	{ (uint8_t *)strname, strlen(strname), 0, 0, 0 }

;; /* BUFPUTSL: optimized bufputs of a string litteral */
;; #define BUFPUTSL(output, literal) \
;; 	bufput(output, literal, sizeof literal - 1)

;; /* bufgrow: increasing the allocated size to the given value */
;; int bufgrow(struct buf *, size_t);

;; /* bufnew: allocation of a new buffer */
;; struct buf *bufnew(size_t) __attribute__ ((malloc));

;; /* bufnullterm: NUL-termination of the string array (making a C-string) */
;; const char *bufcstr(struct buf *);

;; /* bufprefix: compare the beginning of a buffer with a string */
;; int bufprefix(const struct buf *buf, const char *prefix);

;; /* bufput: appends raw data to a buffer */
;; void bufput(struct buf *, const void *, size_t);

;; /* bufputs: appends a NUL-terminated string to a buffer */
;; void bufputs(struct buf *, const char *);

;; /* bufputc: appends a single char to a buffer */
;; void bufputc(struct buf *, int);

;; /* bufrelease: decrease the reference count and free the buffer if needed */
;; void bufrelease(struct buf *);

;; /* bufreset: frees internal data of the buffer */
;; void bufreset(struct buf *);

;; /* bufslurp: removes a given number of bytes from the head of the array */
;; void bufslurp(struct buf *, size_t);

;; /* bufprintf: formatted printing to a buffer */
;; void bufprintf(struct buf *, const char *, ...) __attribute__ ((format (printf, 2, 3)));


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; /* markdown.h - generic markdown parser */
;;

;; /* mkd_autolink - type of autolink */
;; enum mkd_autolink {
;; 	MKDA_NOT_AUTOLINK,	/* used internally when it is not an autolink*/
;; 	MKDA_NORMAL,		/* normal http/http/ftp/mailto/etc link */
;; 	MKDA_EMAIL,			/* e-mail link without explit mailto: */
;; };

(define mkd_autolink
  (_enum
   '(MKDA_NOT_AUTOLINK ;/* used internally when it is not an autolink*/
     MKDA_NORMAL       ;/* normal http/http/ftp/mailto/etc link */
     MKDA_EMAIL        ;/* e-mail link without explit mailto: */
     )))

;; enum mkd_extensions {
;; 	MKDEXT_NO_INTRA_EMPHASIS = (1 << 0),
;; 	MKDEXT_TABLES = (1 << 1),
;; 	MKDEXT_FENCED_CODE = (1 << 2),
;; 	MKDEXT_AUTOLINK = (1 << 3),
;; 	MKDEXT_STRIKETHROUGH = (1 << 4),
;; 	MKDEXT_SPACE_HEADERS = (1 << 6),
;; 	MKDEXT_SUPERSCRIPT = (1 << 7),
;; 	MKDEXT_LAX_SPACING = (1 << 8),
;; };

(define mkd_extensions
  (_enum
   '(MKDEXT_NO_INTRA_EMPHASIS = 0 ;(1 << 0)
     MKDEXT_TABLES = 1            ;(1 << 1)
	 MKDEXT_FENCED_CODE = 2       ;(1 << 2)
	 MKDEXT_AUTOLINK = 4          ;(1 << 3)
	 MKDEXT_STRIKETHROUGH = 8     ;(1 << 4)
	 MKDEXT_SPACE_HEADERS = 16    ;(1 << 6)
	 MKDEXT_SUPERSCRIPT = 32      ;(1 << 7)
	 MKDEXT_LAX_SPACING = 64      ; (1 << 8)
     )))

(define-cstruct _foo
  ([x _int]))

;; /* sd_callbacks - functions for rendering parsed data */
;; struct sd_callbacks {
(define-cstruct _sd_callbacks
  (
  ;;
  ;; /* block level callbacks - NULL skips the block */
  ;;

  ;; void (*blockcode)(struct buf *ob, const struct buf *text, const struct buf *lang, void *opaque);  
  [blockcode (_fun buf-ptr const-buf-ptr const-buf-ptr _voidptr -> _void)]
  ;; void (*blockquote)(struct buf *ob, const struct buf *text, void *opaque);
  [blockquote (_fun buf-ptr const-buf-ptr _voidptr -> _void)]
  ;; void (*blockhtml)(struct buf *ob,const  struct buf *text, void *opaque);
  [blockhtml (_fun buf-ptr const-buf-ptr _voidptr -> _void)]
  ;; void (*header)(struct buf *ob, const struct buf *text, int level, void *opaque);
  [header (_fun buf-ptr const-buf-ptr _int _voidptr -> _void)]
  ;; void (*hrule)(struct buf *ob, void *opaque);
  [hrule (_fun buf-ptr _voidptr -> _void)]
  ;; void (*list)(struct buf *ob, const struct buf *text, int flags, void *opaque);
  [list (_fun buf-ptr const-buf-ptr _int _voidptr -> _void)]
  ;; void (*listitem)(struct buf *ob, const struct buf *text, int flags, void *opaque);
  [listitem (_fun buf-ptr const-buf-ptr _int _voidptr -> _void)]
  ;; void (*paragraph)(struct buf *ob, const struct buf *text, void *opaque);
  [paragraph (_fun buf-ptr const-buf-ptr _voidptr -> _void)]
  ;; void (*table)(struct buf *ob, const struct buf *header, const struct buf *body, void *opaque);
  [table (_fun buf-ptr const-buf-ptr const-buf-ptr _voidptr -> _void)]
  ;; void (*table_row)(struct buf *ob, const struct buf *text, void *opaque);
  [table_row (_fun buf-ptr const-buf-ptr _voidptr -> _void)]
  ;; void (*table_cell)(struct buf *ob, const struct buf *text, int flags, void *opaque);
  [table_cell (_fun buf-ptr const-buf-ptr _int _voidptr -> _void)]

  ;;
  ;; /* span level callbacks - NULL or return 0 prints the span verbatim */
  ;;
  
  ;; int (*autolink)(struct buf *ob, const struct buf *link, enum mkd_autolink type, void *opaque);
  [autolink (_fun buf-ptr const-buf-ptr mkd_autolink _voidptr -> _int)]
  ;; int (*codespan)(struct buf *ob, const struct buf *text, void *opaque);
  [codespan (_fun buf-ptr const-buf-ptr _voidptr -> _int)]
  ;; int (*double_emphasis)(struct buf *ob, const struct buf *text, void *opaque);
  [double_emphasis (_fun buf-ptr const-buf-ptr _voidptr -> _int)]
  ;; int (*emphasis)(struct buf *ob, const struct buf *text, void *opaque);
  [emphasis (_fun buf-ptr const-buf-ptr _voidptr -> _int)]
  ;; int (*image)(struct buf *ob, const struct buf *link, const struct buf *title, const struct buf *alt, void *opaque);
  [image (_fun buf-ptr const-buf-ptr const-buf-ptr const-buf-ptr _voidptr -> _int)]
  ;; int (*linebreak)(struct buf *ob, void *opaque);
  [linebreak (_fun buf-ptr _voidptr -> _int)]
  ;; int (*link)(struct buf *ob, const struct buf *link, const struct buf *title, const struct buf *content, void *opaque);
  [link (_fun buf-ptr const-buf-ptr const-buf-ptr const-buf-ptr _voidptr -> _int)]
  ;; int (*raw_html_tag)(struct buf *ob, const struct buf *tag, void *opaque);
  [raw_html_tag (_fun buf-ptr const-buf-ptr _voidptr -> _int)]
  ;; int (*triple_emphasis)(struct buf *ob, const struct buf *text, void *opaque);
  [triple_emphasis (_fun buf-ptr const-buf-ptr _voidptr -> _int)]
  ;; int (*strikethrough)(struct buf *ob, const struct buf *text, void *opaque);
  [strikethrough (_fun buf-ptr const-buf-ptr _voidptr -> _int)]
  ;; int (*superscript)(struct buf *ob, const struct buf *text, void *opaque);
  [superscript (_fun buf-ptr const-buf-ptr _voidptr -> _int)]

  ;;
  ;; /* low level callbacks - NULL copies input directly into the output */
  ;;

  ;; void (*entity)(struct buf *ob, const struct buf *entity, void *opaque);
  [entity (_fun buf-ptr const-buf-ptr _voidptr -> _void)]
  ;; void (*normal_text)(struct buf *ob, const struct buf *text, void *opaque);
  [normal_text (_fun buf-ptr const-buf-ptr _voidptr -> _void)]

  ;;
  ;; /* header and footer */
  ;;

  ;; void (*doc_header)(struct buf *ob, void *opaque);
  [doc_header (_fun buf-ptr _voidptr -> _void)]
  ;; void (*doc_footer)(struct buf *ob, void *opaque);
  [doc_footer (_fun buf-ptr _voidptr -> _void)]
  ))

;; /*********
;;  * FLAGS *
;;  *********/

;; /* list/listitem flags */
;; #define MKD_LIST_ORDERED	1
;; #define MKD_LI_BLOCK		2  /* <li> containing block data */

(define list/listitem-flags
  (_enum '(MKD_LIST_ORDERED = 1
           MKD_LI_BLOCK = 2)))

;; /**********************
;;  * EXPORTED FUNCTIONS *
;;  **********************/

;; extern struct sd_markdown *
;; sd_markdown_new(
;; 	unsigned int extensions,
;; 	size_t max_nesting,
;; 	const struct sd_callbacks *callbacks,
;; 	void *opaque);

(define-cstruct _sd_markdown
  ([extensions _uint]
   [max_nesting _size_t]
   [callbacks _sd_callbacks-pointer]
   [opaque _voidptr]))

;; extern void
;; sd_markdown_render(struct buf *ob, const uint8_t *document, size_t doc_size, struct sd_markdown *md);

(define-sundown sd_markdown_render
  (_fun (ob : (_ptr io _buf))
        (document : (_ptr i _uint8))
        (doc_size : _size_t)
        (md : (_ptr io _sd_markdown))
        -> _void
        -> ob))

;; extern void
;; sd_markdown_free(struct sd_markdown *md);

(define-sundown sd_markdown_free
  (_fun (md : (_ptr i _sd_markdown))
        -> _void))

;; extern void
;; sd_version(int *major, int *minor, int *revision);

(define-sundown sd_version
  (_fun (major : (_ptr o _int))
        (minor : (_ptr o _int))
        (revision : (_ptr o _int))
        -> _void
        -> (values major minor revision)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (render s)
  (define ib (bufnew (string-length s)))
  (_memcpy ib s)
  (define ob (bufnew 1024))
  (define-values (callbacks options) (sdhtml_renderer 0))
  (define sd_markdown_new 0 16 callbacks options)
  (sd_markdown_render ob id-data ib-size markdown)
  (sd_markdown_free markdown)
  (define result ob)
  (bufrelease ib)
  (bufrelease ob)
  result)
  
