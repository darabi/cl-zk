(in-package :cl-zk)


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
