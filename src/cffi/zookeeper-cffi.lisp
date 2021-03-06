;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-

;;; Generated by Verrazano 0.5 and modified manually by K. Darabi :)

(cl:in-package :cl-user)

(cl:defpackage :zk-cffi
  (:use :cffi)
  (:export "ZOO-SET-LOG-CALLBACK"
           "+ZOO-EPHEMERAL+"
           "ZOO-RECONFIG"
           "+ZOO-PERM-WRITE+"
           "ZOO-STATE"
           "ZOO-AWGET"
           "ZOO-WGET"
           "ZOO-EXISTS"
           "ZOO-AGET"
           "+ZOOKEEPER-WRITE+"
           "+ZOO-CREATED-EVENT+"
           "+ZOO-DELETED-EVENT+"
           "ZOO-CHECK-OP-INIT"
           "ZOOKEEPER-INIT"
           "ZOO-SET-2"
           "ZOO-SET-OP-INIT"
           "ZRWSERVERFOUND"
           "ZNOWATCHER"
           "ZEPHEMERALONLOCALSESSION"
           "ZNOTREADONLY"
           "ZSESSIONMOVED"
           "ZNOTHING"
           "ZCLOSING"
           "ZAUTHFAILED"
           "ZINVALIDACL"
           "ZINVALIDCALLBACK"
           "ZSESSIONEXPIRED"
           "ZNOTEMPTY"
           "ZNODEEXISTS"
           "ZNOCHILDRENFOREPHEMERALS"
           "ZBADVERSION"
           "ZNOAUTH"
           "ZNONODE"
           "ZAPIERROR"
           "ZRECONFIGINPROGRESS"
           "ZNEWCONFIGNOQUORUM"
           "ZINVALIDSTATE"
           "ZBADARGUMENTS"
           "ZOPERATIONTIMEOUT"
           "ZUNIMPLEMENTED"
           "ZMARSHALLINGERROR"
           "ZCONNECTIONLOSS"
           "ZDATAINCONSISTENCY"
           "ZRUNTIMEINCONSISTENCY"
           "ZSYSTEMERROR"
           "ZOK"
           "ZOO-ERRORS"
           "+ZOO-CONNECTED-STATE+"
           "ZOO-AEXISTS"
           "ZOO-GET-CHILDREN"
           "ZOO-WGET-CHILDREN"
           "+ZOO-CHILD-EVENT+"
           "+ZOO-SEQUENCE+"
           "+ZOO-PERM-ALL+"
           "ZERROR"
           "+ZOO-READONLY-STATE+"
           "ZOOKEEPER-GET-CONNECTED-HOST"
           "SOCKLEN-T"
           "SA-DATA"
           "SA-FAMILY"
           "SOCKADDR"
           "SA-FAMILY-T"
           "ZOO-MULTI"
           "+ZOO-AUTH-FAILED-STATE+"
           "ZOO-ACREATE"
           "ZOO-OPEN-ACL-UNSAFE"
           "ZOOKEEPER-CLOSE"
           "ZOO-AGET-ACL"
           "+ZOO-PERM-READ+"
           "ZOOKEEPER-PROCESS"
           "ZOO-GET-CONTEXT"
           "ZOO-REMOVE-WATCHERS"
           "+ZOO-NOTCONNECTED-STATE+"
           "+ZOO-PERM-ADMIN+"
           "ZOO-SET-WATCHER"
           "ZOO-WEXISTS"
           "ZOO-DETERMINISTIC-CONN-ORDER"
           "ZOO-DELETE-OP-INIT"
           "ZOO-AGETCONFIG"
           "ZOO-ANYONE-ID-UNSAFE"
           "ZOO-GET-CHILDREN-2"
           "ZOO-CYCLE-NEXT-SERVER"
           "ZOO-CLIENT-ID"
           "ZOO-GET-LOG-CALLBACK"
           "ZOO-ACREATE-2"
           "STRING-STAT-COMPLETION-T"
           "ZOO-CREATE-OP-INIT"
           "ZOO-AWGET-CHILDREN"
           "ZOO-SET"
           "ZOO-ARECONFIG"
           "ZOO-GET-ACL"
           "+ZOOKEEPER-READ+"
           "ZOO-SET-CONTEXT"
           "ZOO-ASYNC"
           "ZOO-DELETE"
           "ZOO-ADD-AUTH"
           "+ZOO-CONNECTING-STATE+"
           "ZOO-AWGET-CHILDREN-2"
           "ZOO-AMULTI"
           "ZOO-OP-RESULT-T"
           "VALUELEN"
           "VALUE"
           "ERR"
           "ZOO-OP-RESULT"
           "ZOO-OP-T"
           "TYPE"
           "ZOO-OP"
           "ZOO-WGETCONFIG"
           "+ZOO-EXPIRED-SESSION-STATE+"
           "ZOO-WGET-CHILDREN-2"
           "STRING-VECTOR"
           "ZOO-SET-ACL"
           "ZOO-ADELETE"
           "ZOO-AGET-CHILDREN"
           "ZOO-ASET"
           "ZOO-RECV-TIMEOUT"
           "ZOO-GET-CURRENT-SERVER"
           "ZOOKEEPER-INTEREST"
           "TV-USEC"
           "TV-SEC"
           "TIMEVAL"
           "+ZOO-PERM-CREATE+"
           "ZOO-AWEXISTS"
           "ZOO-AGET-CHILDREN-2"
           "STRINGS-STAT-COMPLETION-T"
           "ZOO-SET-LOG-STREAM"
           "FILE"
           "SIZE-T"
           "+ZOO-CHANGED-EVENT+"
           "+ZOO-ASSOCIATING-STATE+"
           "+ZOO-SESSION-EVENT+"
           "ZOO-SET-SERVERS"
           "STRINGS-COMPLETION-T"
           "ZOO-READ-ACL-UNSAFE"
           "ACL-COMPLETION-T"
           "ZOO-AWGETCONFIG"
           "DATA-COMPLETION-T"
           "ZOO-SET-DEBUG-LEVEL"
           "ZOO-LOG-LEVEL-DEBUG"
           "ZOO-LOG-LEVEL-INFO"
           "ZOO-LOG-LEVEL-WARN"
           "ZOO-LOG-LEVEL-ERROR"
           "ZOO-LOG-LEVEL"
           "ZOO-GET"
           "STAT-COMPLETION-T"
           "ZOO-CREATE-2"
           "ZOO-CREATE"
           "ZOO-GETCONFIG"
           "PZXID"
           "NUM-CHILDREN"
           "DATA-LENGTH"
           "EPHEMERAL-OWNER"
           "AVERSION"
           "CVERSION"
           "VERSION"
           "MTIME"
           "CTIME"
           "MZXID"
           "CZXID"
           "STAT"
           "+ZOO-PERM-DELETE+"
           "IS-UNRECOVERABLE"
           "STRING-COMPLETION-T"
           "ZOOKEEPER-INIT-2"
           "PASSWD"
           "CLIENT-ID"
           "CLIENTID-T"
           "INT-64-T"
           "LOG-CALLBACK-FN"
           "+ZOO-NOTWATCHING-EVENT+"
           "ZOO-CREATOR-ALL-ACL"
           "ZOO-AUTH-IDS"
           "ZOO-AREMOVE-WATCHERS"
           "WATCHER-FN"
           "ZWATCHERTYPE-ANY"
           "ZWATCHERTYPE-DATA"
           "ZWATCHERTYPE-CHILDREN"
           "ZOO-WATCHER-TYPE"
           "ZOO-ASET-ACL"
           "VOID-COMPLETION-T"
           "DATA"
           "COUNT"
           "ACL-VECTOR"
           "PERMS"
           "ACL"
           "SCHEME"
           "ID"
           "INT-32-T"
           "ZHANDLE-T"))

(cl:in-package :zk-cffi)

(cl:defun vtable-lookup (pobj indx coff)
  (cl:let ((vptr (cffi:mem-ref pobj :pointer coff)))
    (cffi:mem-aref vptr :pointer (cl:- indx 2))))

(cl:defmacro virtual-funcall (pobj indx coff cl:&body body)
  `(cffi:foreign-funcall-pointer (vtable-lookup ,pobj ,indx ,coff) cl:nil ,@body))

(cffi:defcstruct _zhandle
  )

(cffi::defctype zhandle-t (:struct _zhandle))

(cffi::defctype int-32-t :int)

(cffi:defcstruct id
  (scheme (:pointer :char))
  (id (:pointer :char)))

(cffi:defcstruct acl
  (perms int-32-t)
  (id (:struct id)))

(cffi:defcstruct acl-vector
  (count int-32-t)
  (data :pointer))

(cffi::defctype void-completion-t :pointer)

(cffi:defcfun ("zoo_aset_acl" zoo-aset-acl) :int (zh :pointer) (path :pointer) (version :int)
                                                 (acl :pointer) (arg5 void-completion-t)
                                                 (data :pointer))

(cffi:defcenum zoo-watcher-type
  (:zwatchertype-children 1)
  (:zwatchertype-data 2)
  (:zwatchertype-any 3))

(cffi::defctype watcher-fn :pointer)

(cffi:defcfun ("zoo_aremove_watchers" zoo-aremove-watchers) :int (zh :pointer) (path :pointer)
                                                                 (wtype zoo-watcher-type)
                                                                 (watcher watcher-fn)
                                                                 (watcher-ctx (:pointer :void))
                                                                 (local :int) (completion :pointer)
                                                                 (data :pointer))

(cffi:defcvar ("ZOO_AUTH_IDS" zoo-auth-ids) (:struct id))

(cffi:defcvar ("ZOO_CREATOR_ALL_ACL" zoo-creator-all-acl) (:struct acl-vector))

;;; START of manually modified code

;; /**
;;  * @name ACL Consts
;;  */
;; extern ZOOAPI const int ZOO_PERM_READ;
;; extern ZOOAPI const int ZOO_PERM_WRITE;
;; extern ZOOAPI const int ZOO_PERM_CREATE;
;; extern ZOOAPI const int ZOO_PERM_DELETE;
;; extern ZOOAPI const int ZOO_PERM_ADMIN;
;; extern ZOOAPI const int ZOO_PERM_ALL;
(cffi:defcvar ("ZOO_PERM_READ" +zoo-perm-read+ :read-only t) :int)
(cffi:defcvar ("ZOO_PERM_WRITE" +zoo-perm-write+ :read-only t) :int)
(cffi:defcvar ("ZOO_PERM_CREATE" +zoo-perm-create+ :read-only t) :int)
(cffi:defcvar ("ZOO_PERM_DELETE" +zoo-perm-delete+ :read-only t) :int)
(cffi:defcvar ("ZOO_PERM_ADMIN" +zoo-perm-admin+ :read-only t) :int)
(cffi:defcvar ("ZOO_PERM_ALL" +zoo-perm-all+ :read-only t) :int)

;; /**
;;  * @name Interest Consts
;;  * These constants are used to express interest in an event and to
;;  * indicate to zookeeper which events have occurred. They can
;;  * be ORed together to express multiple interests. These flags are
;;  * used in the interest and event parameters of
;;  * \ref zookeeper_interest and \ref zookeeper_process.
;;  */
;; // @{
;; extern ZOOAPI const int ZOOKEEPER_WRITE;
;; extern ZOOAPI const int ZOOKEEPER_READ;
;; // @}
(cffi:defcvar ("ZOOKEEPER_WRITE" +zookeeper-write+ :read-only t) :int)
(cffi:defcvar ("ZOOKEEPER_READ" +zookeeper-read+ :read-only t) :int)

;; /**
;;  * @name Create Flags
;;  *
;;  * These flags are used by zoo_create to affect node create. They may
;;  * be ORed together to combine effects.
;;  */
;; // @{
;; extern ZOOAPI const int ZOO_EPHEMERAL;
;; extern ZOOAPI const int ZOO_SEQUENCE;
;; // @}
(cffi:defcvar ("ZOO_EPHEMERAL" +zoo-ephemeral+ :read-only t) :int)
(cffi:defcvar ("ZOO_SEQUENCE" +zoo-sequence+ :read-only t) :int)

;; /**
;;  * @name State Consts
;;  * These constants represent the states of a zookeeper connection. They are
;;  * possible parameters of the watcher callback.
;;  */
;; // @{
;; extern ZOOAPI const int ZOO_EXPIRED_SESSION_STATE;
;; extern ZOOAPI const int ZOO_AUTH_FAILED_STATE;
;; extern ZOOAPI const int ZOO_CONNECTING_STATE;
;; extern ZOOAPI const int ZOO_ASSOCIATING_STATE;
;; extern ZOOAPI const int ZOO_CONNECTED_STATE;
;; extern ZOOAPI const int ZOO_READONLY_STATE;
;; extern ZOOAPI const int ZOO_NOTCONNECTED_STATE;
;; // @}
(cffi:defcvar ("ZOO_EXPIRED_SESSION_STATE" +zoo-expired-session-state+ :read-only t) :int)
(cffi:defcvar ("ZOO_AUTH_FAILED_STATE" +zoo-auth-failed-state+ :read-only t) :int)
(cffi:defcvar ("ZOO_CONNECTING_STATE" +zoo-connecting-state+ :read-only t) :int)
(cffi:defcvar ("ZOO_ASSOCIATING_STATE" +zoo-associating-state+ :read-only t) :int)
(cffi:defcvar ("ZOO_CONNECTED_STATE" +zoo-connected-state+ :read-only t) :int)
(cffi:defcvar ("ZOO_READONLY_STATE" +zoo-readonly-state+ :read-only t) :int)
(cffi:defcvar ("ZOO_NOTCONNECTED_STATE" +zoo-notconnected-state+ :read-only t) :int)

;; /**
;;  * @name Watch Types
;;  * These constants indicate the event that caused the watch event. They are
;;  * possible values of the first parameter of the watcher callback.
;;  */
;; // @{
;; /**
;;  * \brief a node has been created.
;;  *
;;  * This is only generated by watches on non-existent nodes. These watches
;;  * are set using \ref zoo_exists.
;;  */
;; extern ZOOAPI const int ZOO_CREATED_EVENT;
;; /**
;;  * \brief a node has been deleted.
;;  *
;;  * This is only generated by watches on nodes. These watches
;;  * are set using \ref zoo_exists and \ref zoo_get.
;;  */
;; extern ZOOAPI const int ZOO_DELETED_EVENT;
;; /**
;;  * \brief a node has changed.
;;  *
;;  * This is only generated by watches on nodes. These watches
;;  * are set using \ref zoo_exists and \ref zoo_get.
;;  */
;; extern ZOOAPI const int ZOO_CHANGED_EVENT;
;; /**
;;  * \brief a change as occurred in the list of children.
;;  *
;;  * This is only generated by watches on the child list of a node. These watches
;;  * are set using \ref zoo_get_children or \ref zoo_get_children2.
;;  */
;; extern ZOOAPI const int ZOO_CHILD_EVENT;
;; /**
;;  * \brief a session has been lost.
;;  *
;;  * This is generated when a client loses contact or reconnects with a server.
;;  */
;; extern ZOOAPI const int ZOO_SESSION_EVENT;
;;
;; /**
;;  * \brief a watch has been removed.
;;  *
;;  * This is generated when the server for some reason, probably a resource
;;  * constraint, will no longer watch a node for a client.
;;  */
;; extern ZOOAPI const int ZOO_NOTWATCHING_EVENT;
;; // @}
(cffi:defcvar ("ZOO_CREATED_EVENT" +zoo-created-event+ :read-only t) :int)
(cffi:defcvar ("ZOO_DELETED_EVENT" +zoo-deleted-event+ :read-only t) :int)
(cffi:defcvar ("ZOO_CHANGED_EVENT" +zoo-changed-event+ :read-only t) :int)
(cffi:defcvar ("ZOO_CHILD_EVENT" +zoo-child-event+ :read-only t) :int)
(cffi:defcvar ("ZOO_SESSION_EVENT" +zoo-session-event+ :read-only t) :int)
(cffi:defcvar ("ZOO_NOTWATCHING_EVENT" +zoo-notwatching-event+ :read-only t) :int)

;;; END of manually modified code

(cffi::defctype log-callback-fn :pointer)

(cffi::defctype int-64-t :long)

(cffi:defcstruct clientid-t
  (client-id int-64-t)
  (passwd :char :count 16))

(cffi:defcfun ("zookeeper_init2" zookeeper-init-2) :pointer
  "/**
 * \brief create a handle to communicate with zookeeper.
 *
 * This function is identical to \ref zookeeper_init except it allows one
 * to specify an additional callback to be used for all logging for that
 * specific connection. For more details on the logging callback see
 * \ref zoo_get_log_callback and \ref zoo_set_log_callback.
 *
 * This method creates a new handle and a zookeeper session that corresponds
 * to that handle. Session establishment is asynchronous, meaning that the
 * session should not be considered established until (and unless) an
 * event of state ZOO_CONNECTED_STATE is received.
 * \param host comma separated host:port pairs, each corresponding to a zk
 *   server. e.g. 127.0.0.1:3000,127.0.0.1:3001,127.0.0.1:3002
 * \param fn the global watcher callback function. When notifications are
 *   triggered this function will be invoked.
 * \param clientid the id of a previously established session that this
 *   client will be reconnecting to. Pass 0 if not reconnecting to a previous
 *   session. Clients can access the session id of an established, valid,
 *   connection by calling \ref zoo_client_id. If the session corresponding to
 *   the specified clientid has expired, or if the clientid is invalid for
 *   any reason, the returned zhandle_t will be invalid -- the zhandle_t
 *   state will indicate the reason for failure (typically
 *   ZOO_EXPIRED_SESSION_STATE).
 * \param context the handback object that will be associated with this instance
 *   of zhandle_t. Application can access it (for example, in the watcher
 *   callback) using \ref zoo_get_context. The object is not used by zookeeper
 *   internally and can be null.
 * \param flags reserved for future use. Should be set to zero.
 * \param log_callback All log messages will be passed to this callback function.
 *   For more details see \ref zoo_get_log_callback and \ref zoo_set_log_callback.
 * \return a pointer to the opaque zhandle structure. If it fails to create
 * a new zhandle the function returns NULL and the errno variable
 * indicates the reason.
 */
"
  (host :string) (fn watcher-fn)
  (recv-timeout :int) (clientid :pointer)
  (context (:pointer :void)) (flags :int)
  (log-callback log-callback-fn))

(cffi::defctype string-completion-t :pointer)

(cffi:defcfun ("is_unrecoverable" is-unrecoverable) :int (zh :pointer))

(cffi:defcstruct stat
  (czxid int-64-t)
  (mzxid int-64-t)
  (ctime int-64-t)
  (mtime int-64-t)
  (version int-32-t)
  (cversion int-32-t)
  (aversion int-32-t)
  (ephemeral-owner int-64-t)
  (data-length int-32-t)
  (num-children int-32-t)
  (pzxid int-64-t))

(cffi:defcfun ("zoo_getconfig" zoo-getconfig) :int (zh :pointer) (watch :int)
                                                   (buffer (:pointer :char))
                                                   (buffer-len (:pointer :int)) (stat :pointer))

(cffi:defcfun ("zoo_create" zoo-create) :int
  "/**
 * \brief create a node synchronously.
 *
 * This method will create a node in ZooKeeper. A node can only be created if
 * it does not already exists. The Create Flags affect the creation of nodes.
 * If ZOO_EPHEMERAL flag is set, the node will automatically get removed if the
 * client session goes away. If the ZOO_SEQUENCE flag is set, a unique
 * monotonically increasing sequence number is appended to the path name.
 *
 * \param zh the zookeeper handle obtained by a call to \ref zookeeper_init
 * \param path The name of the node. Expressed as a file name with slashes
 * separating ancestors of the node.
 * \param value The data to be stored in the node.
 * \param valuelen The number of bytes in data. To set the data to be NULL use
 * value as NULL and valuelen as -1.
 * \param acl The initial ACL of the node. The ACL must not be null or empty.
 * \param flags this parameter can be set to 0 for normal create or an OR
 *    of the Create Flags
 * \param path_buffer Buffer which will be filled with the path of the
 *    new node (this might be different than the supplied path
 *    because of the ZOO_SEQUENCE flag).  The path string will always be
 *    null-terminated. This parameter may be NULL if path_buffer_len = 0.
 * \param path_buffer_len Size of path buffer; if the path of the new
 *    node (including space for the null terminator) exceeds the buffer size,
 *    the path string will be truncated to fit.  The actual path of the
 *    new node in the server will not be affected by the truncation.
 *    The path string will always be null-terminated.
 * \return  one of the following codes are returned:
 * ZOK operation completed successfully
 * ZNONODE the parent node does not exist.
 * ZNODEEXISTS the node already exists
 * ZNOAUTH the client does not have permission.
 * ZNOCHILDRENFOREPHEMERALS cannot create children of ephemeral nodes.
 * ZBADARGUMENTS - invalid input parameters
 * ZINVALIDSTATE - zhandle state is either ZOO_SESSION_EXPIRED_STATE or ZOO_AUTH_FAILED_STATE
 * ZMARSHALLINGERROR - failed to marshall a request; possibly, out of memory
 */
"
  (zh :pointer) (path :string) (value :pointer)
  (valuelen :int) (acl :pointer) (flags :int)
  (path-buffer (:pointer :char)) (path-buffer-len :int))

(cffi:defcfun ("zoo_create2" zoo-create-2) :int (zh :pointer) (path :string) (value :pointer)
                                                (valuelen :int) (acl :pointer) (flags :int)
                                                (path-buffer (:pointer :char))
                                                (path-buffer-len :int) (stat :pointer))

(cffi::defctype stat-completion-t :pointer)

(cffi:defcfun ("zoo_get" zoo-get) :int (zh :pointer) (path :string) (watch :int)
                                       (buffer (:pointer :char)) (buffer-len (:pointer :int))
                                       (stat :pointer))

(cffi:defcenum zoo-log-level
  (:zoo-log-level-error 1)
  (:zoo-log-level-warn 2)
  (:zoo-log-level-info 3)
  (:zoo-log-level-debug 4))

(cffi:defcfun ("zoo_set_debug_level" zoo-set-debug-level) :void (log-level zoo-log-level))

(cffi::defctype data-completion-t :pointer)

(cffi:defcfun ("zoo_awgetconfig" zoo-awgetconfig) :int (zh :pointer) (watcher watcher-fn)
                                                       (watcher-ctx (:pointer :void))
                                                       (completion data-completion-t)
                                                       (data :pointer))

(cffi::defctype acl-completion-t :pointer)

(cffi:defcvar ("ZOO_READ_ACL_UNSAFE" zoo-read-acl-unsafe) (:struct acl-vector))

(cffi::defctype strings-completion-t :pointer)

(cffi:defcfun ("zoo_set_servers" zoo-set-servers) :int (zh :pointer) (hosts :pointer))

(cffi:defcstruct _io-marker
  (_next :pointer)
  (_sbuf :pointer)
  (_pos :int))

(cffi::defctype _-off-t :long)

(cffi::defctype _io-lock-t :void)

(cffi::defctype _-off-64-t :long)

(cffi::defctype size-t :unsigned-long)

(cffi:defcstruct _io-file
  (_flags :int)
  (_io-read-ptr (:pointer :char))
  (_io-read-end (:pointer :char))
  (_io-read-base (:pointer :char))
  (_io-write-base (:pointer :char))
  (_io-write-ptr (:pointer :char))
  (_io-write-end (:pointer :char))
  (_io-buf-base (:pointer :char))
  (_io-buf-end (:pointer :char))
  (_io-save-base (:pointer :char))
  (_io-backup-base (:pointer :char))
  (_io-save-end (:pointer :char))
  (_markers :pointer)
  (_chain :pointer)
  (_fileno :int)
  (_flags-2 :int)
  (_old-offset _-off-t)
  (_cur-column :unsigned-short)
  (_vtable-offset :char)
  (_shortbuf :char :count 1)
  (_lock :pointer)
  (_offset _-off-64-t)
  (_-pad-1 (:pointer :void))
  (_-pad-2 (:pointer :void))
  (_-pad-3 (:pointer :void))
  (_-pad-4 (:pointer :void))
  (_-pad-5 size-t)
  (_mode :int)
  (_unused-2 :char :count 20))

(cffi::defctype file (:struct _io-file))

(cffi:defcfun ("zoo_set_log_stream" zoo-set-log-stream) :void (log-stream :pointer))

(cffi::defctype strings-stat-completion-t :pointer)

(cffi:defcfun ("zoo_aget_children2" zoo-aget-children-2) :int (zh :pointer) (path :pointer)
                                                              (watch :int)
                                                              (completion strings-stat-completion-t)
                                                              (data :pointer))

(cffi:defcfun ("zoo_awexists" zoo-awexists) :int (zh :pointer) (path :pointer) (watcher watcher-fn)
                                                 (watcher-ctx (:pointer :void))
                                                 (completion stat-completion-t) (data :pointer))

(cffi::defctype _-time-t :long)

(cffi::defctype _-suseconds-t :long)

(cffi:defcstruct timeval
  (tv-sec _-time-t)
  (tv-usec _-suseconds-t))

(cffi:defcfun ("zookeeper_interest" zookeeper-interest) :int (zh :pointer) (fd (:pointer :int))
                                                             (interest (:pointer :int))
                                                             (tv :pointer))

(cffi:defcfun ("zoo_get_current_server" zoo-get-current-server) :pointer (zh :pointer))

(cffi:defcfun ("zoo_recv_timeout" zoo-recv-timeout) :int (zh :pointer))

(cffi:defcfun ("zoo_aset" zoo-aset) :int (zh :pointer) (path :pointer) (buffer :pointer)
                                         (buflen :int) (version :int) (completion stat-completion-t)
                                         (data :pointer))

(cffi:defcfun ("zoo_aget_children" zoo-aget-children) :int (zh :pointer) (path :pointer)
                                                           (watch :int)
                                                           (completion strings-completion-t)
                                                           (data :pointer))

(cffi:defcfun ("zoo_adelete" zoo-adelete) :int (zh :pointer) (path :pointer) (version :int)
                                               (completion void-completion-t) (data :pointer))

(cffi:defcfun ("zoo_set_acl" zoo-set-acl) :int (zh :pointer) (path :pointer) (version :int)
                                               (acl :pointer))

(cffi:defcstruct string-vector
  (count int-32-t)
  (data :pointer))

(cffi:defcfun ("zoo_wget_children2" zoo-wget-children-2) :int (zh :pointer) (path :pointer)
                                                              (watcher watcher-fn)
                                                              (watcher-ctx (:pointer :void))
                                                              (strings :pointer) (stat :pointer))


(cffi:defcfun ("zoo_wgetconfig" zoo-wgetconfig) :int (zh :pointer) (watcher watcher-fn)
                                                     (watcher-ctx (:pointer :void))
                                                     (buffer (:pointer :char))
                                                     (buffer-len (:pointer :int)) (stat :pointer))

;;; Skipping anonymous composite type #<UNION <anonymous> {100BF63613}>

;; START of manual correction

;; TODO: the union inside the zoo-op struct (defined in zookeeper.h)
;; was not processed correctly by Verrazano. Do we need it?  Two of
;; the operations (create-op and delete-op) and the union
;; zoo-operation are declared here, manually:

(cffi:defcstruct create-op
  (path :pointer)
  (data :pointer)
  (datalen :int)
  (buf :pointer)
  (buflen :int)
  (acl (:struct acl-vector))
  (flags :int))

(cffi:defcstruct delete-op
  (path :pointer)
  (version :int))

(cffi:defcunion zoo-operation
  (create-op (:struct create-op))
  (delete-op (:struct delete-op)))

(cffi:defcstruct zoo-op
  (type :int)
  (op (:union zoo-operation)))

;; END of manual code

(cffi::defctype zoo-op-t (:struct zoo-op))

(cffi:defcstruct zoo-op-result
  (err :int)
  (value (:pointer :char))
  (valuelen :int)
  (stat :pointer))

(cffi::defctype zoo-op-result-t (:struct zoo-op-result))

(cffi:defcfun ("zoo_amulti" zoo-amulti) :int (zh :pointer) (count :int) (ops :pointer)
                                             (results :pointer) (arg5 void-completion-t)
                                             (data :pointer))

(cffi:defcfun ("zoo_awget_children2" zoo-awget-children-2) :int (zh :pointer) (path :pointer)
                                                                (watcher watcher-fn)
                                                                (watcher-ctx (:pointer :void))
                                                                (completion strings-stat-completion-t)
                                                                (data :pointer))

(cffi:defcfun ("zoo_add_auth" zoo-add-auth) :int (zh :pointer) (scheme :pointer) (cert :pointer)
                                                 (cert-len :int) (completion void-completion-t)
                                                 (data :pointer))

(cffi:defcfun ("zoo_delete" zoo-delete) :int (zh :pointer) (path :pointer) (version :int))

(cffi:defcfun ("zoo_async" zoo-async) :int (zh :pointer) (path :pointer)
                                           (completion string-completion-t) (data :pointer))

(cffi:defcfun ("zoo_set_context" zoo-set-context) :void (zh :pointer) (context (:pointer :void)))


(cffi:defcfun ("zoo_get_acl" zoo-get-acl) :int (zh :pointer) (path :pointer) (acl :pointer)
                                               (stat :pointer))

(cffi:defcfun ("zoo_areconfig" zoo-areconfig) :int (zh :pointer) (joining :pointer)
                                                   (leaving :pointer) (members :pointer)
                                                   (version int-64-t) (dc data-completion-t)
                                                   (data :pointer))

(cffi:defcfun ("zoo_set" zoo-set) :int (zh :pointer) (path :string) (buffer :pointer) (buflen :int)
                                       (version :int))

(cffi:defcfun ("zoo_awget_children" zoo-awget-children) :int (zh :pointer) (path :pointer)
                                                             (watcher watcher-fn)
                                                             (watcher-ctx (:pointer :void))
                                                             (completion strings-completion-t)
                                                             (data :pointer))

(cffi:defcfun ("zoo_create_op_init" zoo-create-op-init) :void (op :pointer) (path :pointer)
                                                              (value :pointer) (valuelen :int)
                                                              (acl :pointer) (flags :int)
                                                              (path-buffer (:pointer :char))
                                                              (path-buffer-len :int))

(cffi::defctype string-stat-completion-t :pointer)

(cffi:defcfun ("zoo_acreate2" zoo-acreate-2) :int (zh :pointer) (path :pointer) (value :pointer)
                                                  (valuelen :int) (acl :pointer) (flags :int)
                                                  (completion string-stat-completion-t)
                                                  (data :pointer))

(cffi:defcfun ("zoo_get_log_callback" zoo-get-log-callback) log-callback-fn (zh :pointer))

(cffi:defcfun ("zoo_client_id" zoo-client-id) :pointer (zh :pointer))

(cffi:defcfun ("zoo_cycle_next_server" zoo-cycle-next-server) :void (zh :pointer))

(cffi:defcfun ("zoo_get_children2" zoo-get-children-2) :int (zh :pointer) (path :pointer)
                                                            (watch :int) (strings :pointer)
                                                            (stat :pointer))

(cffi:defcvar ("ZOO_ANYONE_ID_UNSAFE" zoo-anyone-id-unsafe) (:struct id))

(cffi:defcfun ("zoo_agetconfig" zoo-agetconfig) :int (zh :pointer) (watch :int)
                                                     (completion data-completion-t) (data :pointer))

(cffi:defcfun ("zoo_delete_op_init" zoo-delete-op-init) :void (op :pointer) (path :pointer)
                                                              (version :int))

(cffi:defcfun ("zoo_deterministic_conn_order" zoo-deterministic-conn-order) :void (yes-or-no :int))

(cffi:defcfun ("zoo_wexists" zoo-wexists) :int (zh :pointer) (path :pointer) (watcher watcher-fn)
                                               (watcher-ctx (:pointer :void)) (stat :pointer))

(cffi:defcfun ("zoo_set_watcher" zoo-set-watcher) watcher-fn (zh :pointer) (new-fn watcher-fn))


(cffi:defcfun ("zoo_remove_watchers" zoo-remove-watchers) :int (zh :pointer) (path :pointer)
                                                               (wtype zoo-watcher-type)
                                                               (watcher watcher-fn)
                                                               (watcher-ctx (:pointer :void))
                                                               (local :int))

(cffi:defcfun ("zoo_get_context" zoo-get-context) :pointer (zh :pointer))

(cffi:defcfun ("zookeeper_process" zookeeper-process) :int (zh :pointer) (events :int))

(cffi:defcfun ("zoo_aget_acl" zoo-aget-acl) :int (zh :pointer) (path :pointer)
                                                 (completion acl-completion-t) (data :pointer))

(cffi:defcfun ("zookeeper_close" zookeeper-close) :int
  "/**
 * \brief close the zookeeper handle and free up any resources.
 *
 * After this call, the client session will no longer be valid. The function
 * will flush any outstanding send requests before return. As a result it may
 * block.
 *
 * This method should only be called only once on a zookeeper handle. Calling
 * twice will cause undefined (and probably undesirable behavior). Calling any other
 * zookeeper method after calling close is undefined behaviour and should be avoided.
 *
 * \param zh the zookeeper handle obtained by a call to \ref zookeeper_init
 * \return a result code. Regardless of the error code returned, the zhandle
 * will be destroyed and all resources freed.
 *
 * ZOK - success
 * ZBADARGUMENTS - invalid input parameters
 * ZMARSHALLINGERROR - failed to marshall a request; possibly, out of memory
 * ZOPERATIONTIMEOUT - failed to flush the buffers within the specified timeout.
 * ZCONNECTIONLOSS - a network error occured while attempting to send request to server
 * ZSYSTEMERROR -- a system (OS) error occured; it's worth checking errno to get details
 */
"
  (zh :pointer))

(cffi:defcvar ("ZOO_OPEN_ACL_UNSAFE" zoo-open-acl-unsafe) (:struct acl-vector))

(cffi:defcfun ("zoo_acreate" zoo-acreate) :int (zh :pointer) (path :pointer) (value :pointer)
                                               (valuelen :int) (acl :pointer) (flags :int)
                                               (completion string-completion-t) (data :pointer))

(cffi:defcfun ("zoo_multi" zoo-multi) :int (zh :pointer) (count :int) (ops :pointer)
                                           (results :pointer))

(cffi::defctype sa-family-t :unsigned-short)

(cffi:defcstruct sockaddr
  (sa-family sa-family-t)
  (sa-data :char :count 14))

(cffi::defctype _-socklen-t :unsigned-int)

(cffi::defctype socklen-t _-socklen-t)

(cffi:defcfun ("zookeeper_get_connected_host" zookeeper-get-connected-host) :pointer (zh :pointer)
                                                                                     (addr :pointer)
                                                                                     (addr-len :pointer))

(cffi:defcfun ("zerror" zerror) :string (c :int))

(cffi:defcfun ("zoo_wget_children" zoo-wget-children) :int (zh :pointer) (path :pointer)
                                                           (watcher watcher-fn)
                                                           (watcher-ctx (:pointer :void))
                                                           (strings :pointer))

(cffi:defcfun ("zoo_get_children" zoo-get-children) :int (zh :pointer) (path :string) (watch :int)
                                                         (strings :pointer))

(cffi:defcfun ("zoo_aexists" zoo-aexists) :int (zh :pointer) (path :pointer) (watch :int)
                                               (completion stat-completion-t) (data :pointer))

(cffi:defcenum zoo-errors
  (:zok 0)
  (:zsystemerror -1)
  (:zruntimeinconsistency -2)
  (:zdatainconsistency -3)
  (:zconnectionloss -4)
  (:zmarshallingerror -5)
  (:zunimplemented -6)
  (:zoperationtimeout -7)
  (:zbadarguments -8)
  (:zinvalidstate -9)
  (:znewconfignoquorum -13)
  (:zreconfiginprogress -14)
  (:zapierror -100)
  (:znonode -101)
  (:znoauth -102)
  (:zbadversion -103)
  (:znochildrenforephemerals -108)
  (:znodeexists -110)
  (:znotempty -111)
  (:zsessionexpired -112)
  (:zinvalidcallback -113)
  (:zinvalidacl -114)
  (:zauthfailed -115)
  (:zclosing -116)
  (:znothing -117)
  (:zsessionmoved -118)
  (:znotreadonly -119)
  (:zephemeralonlocalsession -120)
  (:znowatcher -121)
  (:zrwserverfound -122))

(cffi:defcfun ("zoo_set_op_init" zoo-set-op-init) :void (op :pointer) (path :pointer)
                                                        (buffer :pointer) (buflen :int)
                                                        (version :int) (stat :pointer))

(cffi:defcfun ("zoo_set2" zoo-set-2) :int (zh :pointer) (path :pointer) (buffer :pointer)
                                          (buflen :int) (version :int) (stat :pointer))

(cffi:defcfun ("zookeeper_init" zookeeper-init) :pointer
  "/**
 * \brief create a handle to used communicate with zookeeper.
 *
 * This method creates a new handle and a zookeeper session that corresponds
 * to that handle. Session establishment is asynchronous, meaning that the
 * session should not be considered established until (and unless) an
 * event of state ZOO_CONNECTED_STATE is received.
 * \param host comma separated host:port pairs, each corresponding to a zk
 *   server. e.g. 127.0.0.1:3000,127.0.0.1:3001,127.0.0.1:3002
 * \param fn the global watcher callback function. When notifications are
 *   triggered this function will be invoked.
 * \param clientid the id of a previously established session that this
 *   client will be reconnecting to. Pass 0 if not reconnecting to a previous
 *   session. Clients can access the session id of an established, valid,
 *   connection by calling \ref zoo_client_id. If the session corresponding to
 *   the specified clientid has expired, or if the clientid is invalid for
 *   any reason, the returned zhandle_t will be invalid -- the zhandle_t
 *   state will indicate the reason for failure (typically
 *   ZOO_EXPIRED_SESSION_STATE).
 * \param context the handback object that will be associated with this instance
 *   of zhandle_t. Application can access it (for example, in the watcher
 *   callback) using \ref zoo_get_context. The object is not used by zookeeper
 *   internally and can be null.
 * \param flags reserved for future use. Should be set to zero.
 * \return a pointer to the opaque zhandle structure. If it fails to create
 * a new zhandle the function returns NULL and the errno variable
 * indicates the reason.
 */
"
  (host :string) (fn watcher-fn)
  (recv-timeout :int) (clientid :pointer)
  (context (:pointer :void)) (flags :int))

(cffi:defcfun ("zoo_check_op_init" zoo-check-op-init) :void (op :pointer) (path :pointer)
                                                            (version :int))

(cffi:defcfun ("zoo_aget" zoo-aget) :int (zh :pointer) (path :pointer) (watch :int)
                                         (completion data-completion-t) (data :pointer))

(cffi:defcfun ("zoo_exists" zoo-exists) :int (zh :pointer) (path :pointer) (watch :int)
                                             (stat :pointer))

(cffi:defcfun ("zoo_wget" zoo-wget) :int (zh :pointer) (path :pointer) (watcher watcher-fn)
                                         (watcher-ctx (:pointer :void)) (buffer (:pointer :char))
                                         (buffer-len (:pointer :int)) (stat :pointer))

(cffi:defcfun ("zoo_awget" zoo-awget) :int (zh :pointer) (path :pointer) (watcher watcher-fn)
                                           (watcher-ctx (:pointer :void))
                                           (completion data-completion-t) (data :pointer))

(cffi:defcfun ("zoo_state" zoo-state) :int (zh :pointer))

(cffi:defcfun ("zoo_reconfig" zoo-reconfig) :int (zh :pointer) (joining :pointer) (leaving :pointer)
                                                 (members :pointer) (version int-64-t)
                                                 (buffer (:pointer :char))
                                                 (buffer-len (:pointer :int)) (stat :pointer))

(cffi:defcfun ("zoo_set_log_callback" zoo-set-log-callback) :void
  "/**
 * \brief sets the callback to be used by the library for logging
 *
 * Setting this callback has the effect of overriding the default log stream.
 * Zookeeper will first try to use a per-connection callback if available
 * and if not, will fallback to using the logging stream. Passing in NULL
 * resets the callback and will cause it to then fallback to using the logging
 * stream as described in \ref zoo_set_log_stream.
 *
 * Note: The provided callback will be invoked by multiple threads and therefore
 * it needs to be thread-safe.
 */
"
  (zh :pointer)
  (callback log-callback-fn))
