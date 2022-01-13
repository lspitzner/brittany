{-# OPTIONS_GHC -Wno-implicit-prelude #-}

module Language.Haskell.Brittany.Internal.ParseModule where

import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Trans.Except as Except
import qualified GHC
import qualified GHC.ByteOrder
import qualified GHC.Data.Bag
import qualified GHC.Data.StringBuffer
import qualified GHC.Driver.Session
import qualified GHC.Parser.Header
import qualified GHC.Platform
import qualified GHC.Settings
import qualified GHC.Types.SrcLoc
import qualified GHC.Utils.Error
import qualified GHC.Utils.Fingerprint
import qualified Language.Haskell.GHC.ExactPrint.Parsers as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint

-- | Parses a Haskell module. Although this nominally requires IO, it is
-- morally pure. It should have no observable effects.
parseModule
  :: IO.MonadIO io
  => [String]
  -> FilePath
  -> (GHC.Driver.Session.DynFlags -> io (Either String a))
  -> String
  -> io (Either String (ExactPrint.Anns, GHC.ParsedSource, a))
parseModule arguments1 filePath checkDynFlags string = Except.runExceptT $ do
  let
    dynFlags1 = GHC.Driver.Session.gopt_set
      -- It feels like this should be either @Sf_Ignore@ or @Sf_None@, but both
      -- of those modes have trouble parsing safe imports (@import safe ...@).
      -- Neither passing in @"-XUnsafe"@ as a command line argument nor having
      -- @{-# LANGUAGE Unsafe #-}@ in the source file seem to help.
      initialDynFlags
        { GHC.Driver.Session.safeHaskell = GHC.Driver.Session.Sf_Unsafe
        }
      GHC.Driver.Session.Opt_KeepRawTokenStream
  (dynFlags2, leftovers1, _) <-
    GHC.Driver.Session.parseDynamicFlagsCmdLine dynFlags1
      $ fmap GHC.Types.SrcLoc.noLoc arguments1
  handleLeftovers leftovers1
  let
    stringBuffer = GHC.Data.StringBuffer.stringToStringBuffer string
    arguments2 = GHC.Parser.Header.getOptions dynFlags2 stringBuffer filePath
  (dynFlags3, leftovers2, _) <- GHC.Driver.Session.parseDynamicFilePragma
    dynFlags2
    arguments2
  handleLeftovers leftovers2
  dynFlagsResult <- Except.ExceptT $ checkDynFlags dynFlags3
  let
    parseResult =
      ExactPrint.parseModuleFromStringInternal dynFlags3 filePath string
  case parseResult of
    Left errorMessages -> handleErrorMessages errorMessages
    Right (anns, parsedSource) -> pure (anns, parsedSource, dynFlagsResult)

handleLeftovers
  :: Monad m => [GHC.Types.SrcLoc.Located String] -> Except.ExceptT String m ()
handleLeftovers leftovers =
  Monad.unless (null leftovers) . Except.throwE $ "leftovers: " <> show
    (fmap GHC.Types.SrcLoc.unLoc leftovers)

handleErrorMessages
  :: Monad m => GHC.Utils.Error.ErrorMessages -> Except.ExceptT String m a
handleErrorMessages =
  Except.throwE . mappend "errorMessages: " . show . GHC.Data.Bag.bagToList

initialDynFlags :: GHC.Driver.Session.DynFlags
initialDynFlags = GHC.Driver.Session.defaultDynFlags initialSettings initialLlvmConfig

initialSettings :: GHC.Driver.Session.Settings
initialSettings = GHC.Driver.Session.Settings
  { GHC.Driver.Session.sGhcNameVersion = initialGhcNameVersion
  , GHC.Driver.Session.sFileSettings = initialFileSettings
  , GHC.Driver.Session.sTargetPlatform = initialTargetPlatform
  , GHC.Driver.Session.sToolSettings = initialToolSettings
  , GHC.Driver.Session.sPlatformMisc = initialPlatformMisc
  , GHC.Driver.Session.sPlatformConstants = initialPlatformConstants
  , GHC.Driver.Session.sRawSettings = []
  }

initialFileSettings :: GHC.Driver.Session.FileSettings
initialFileSettings = GHC.Driver.Session.FileSettings
  { GHC.Driver.Session.fileSettings_ghciUsagePath = ""
  , GHC.Driver.Session.fileSettings_ghcUsagePath = ""
  , GHC.Driver.Session.fileSettings_globalPackageDatabase = ""
  , GHC.Driver.Session.fileSettings_tmpDir = ""
  , GHC.Driver.Session.fileSettings_toolDir = Nothing
  , GHC.Driver.Session.fileSettings_topDir = ""
  }

initialGhcNameVersion :: GHC.Driver.Session.GhcNameVersion
initialGhcNameVersion = GHC.Driver.Session.GhcNameVersion
  { GHC.Driver.Session.ghcNameVersion_programName = ""
  , GHC.Driver.Session.ghcNameVersion_projectVersion = ""
  }

initialPlatformMisc :: GHC.Driver.Session.PlatformMisc
initialPlatformMisc = GHC.Driver.Session.PlatformMisc
  { GHC.Driver.Session.platformMisc_ghcDebugged = False
  , GHC.Driver.Session.platformMisc_ghcRTSWays = ""
  , GHC.Driver.Session.platformMisc_ghcRtsWithLibdw = False
  , GHC.Driver.Session.platformMisc_ghcThreaded = False
  , GHC.Driver.Session.platformMisc_ghcWithInterpreter = False
  , GHC.Driver.Session.platformMisc_ghcWithSMP = False
  , GHC.Driver.Session.platformMisc_libFFI = False
  , GHC.Driver.Session.platformMisc_llvmTarget = ""
  , GHC.Driver.Session.platformMisc_targetPlatformString = ""
  }

initialLlvmConfig :: GHC.Driver.Session.LlvmConfig
initialLlvmConfig = GHC.Driver.Session.LlvmConfig
  { GHC.Driver.Session.llvmPasses = []
  , GHC.Driver.Session.llvmTargets = []
  }

initialPlatformConstants :: GHC.Settings.PlatformConstants
initialPlatformConstants = GHC.Settings.PlatformConstants
  { GHC.Settings.pc_AP_STACK_SPLIM = 0
  , GHC.Settings.pc_BITMAP_BITS_SHIFT = 0
  , GHC.Settings.pc_BLOCK_SIZE = 0
  , GHC.Settings.pc_BLOCKS_PER_MBLOCK = 0
  , GHC.Settings.pc_CINT_SIZE = 0
  , GHC.Settings.pc_CLONG_LONG_SIZE = 0
  , GHC.Settings.pc_CLONG_SIZE = 0
  , GHC.Settings.pc_CONTROL_GROUP_CONST_291 = 0
  , GHC.Settings.pc_DYNAMIC_BY_DEFAULT = False
  , GHC.Settings.pc_ILDV_CREATE_MASK = 0
  , GHC.Settings.pc_ILDV_STATE_CREATE = 0
  , GHC.Settings.pc_ILDV_STATE_USE = 0
  , GHC.Settings.pc_LDV_SHIFT = 0
  , GHC.Settings.pc_MAX_CHARLIKE = 0
  , GHC.Settings.pc_MAX_Double_REG = 0
  , GHC.Settings.pc_MAX_Float_REG = 0
  , GHC.Settings.pc_MAX_INTLIKE = 0
  , GHC.Settings.pc_MAX_Long_REG = 0
  , GHC.Settings.pc_MAX_Real_Double_REG = 0
  , GHC.Settings.pc_MAX_Real_Float_REG = 0
  , GHC.Settings.pc_MAX_Real_Long_REG = 0
  , GHC.Settings.pc_MAX_Real_Vanilla_REG = 0
  , GHC.Settings.pc_MAX_Real_XMM_REG = 0
  , GHC.Settings.pc_MAX_SPEC_AP_SIZE = 0
  , GHC.Settings.pc_MAX_SPEC_SELECTEE_SIZE = 0
  , GHC.Settings.pc_MAX_Vanilla_REG = 0
  , GHC.Settings.pc_MAX_XMM_REG = 0
  , GHC.Settings.pc_MIN_CHARLIKE = 0
  , GHC.Settings.pc_MIN_INTLIKE = 0
  , GHC.Settings.pc_MIN_PAYLOAD_SIZE = 0
  , GHC.Settings.pc_MUT_ARR_PTRS_CARD_BITS = 0
  , GHC.Settings.pc_OFFSET_bdescr_blocks = 0
  , GHC.Settings.pc_OFFSET_bdescr_flags = 0
  , GHC.Settings.pc_OFFSET_bdescr_free = 0
  , GHC.Settings.pc_OFFSET_bdescr_start = 0
  , GHC.Settings.pc_OFFSET_Capability_r = 0
  , GHC.Settings.pc_OFFSET_CostCentreStack_mem_alloc = 0
  , GHC.Settings.pc_OFFSET_CostCentreStack_scc_count = 0
  , GHC.Settings.pc_OFFSET_StgArrBytes_bytes = 0
  , GHC.Settings.pc_OFFSET_stgEagerBlackholeInfo = 0
  , GHC.Settings.pc_OFFSET_StgEntCounter_allocd = 0
  , GHC.Settings.pc_OFFSET_StgEntCounter_allocs = 0
  , GHC.Settings.pc_OFFSET_StgEntCounter_entry_count = 0
  , GHC.Settings.pc_OFFSET_StgEntCounter_link = 0
  , GHC.Settings.pc_OFFSET_StgEntCounter_registeredp = 0
  , GHC.Settings.pc_OFFSET_StgFunInfoExtraFwd_arity = 0
  , GHC.Settings.pc_OFFSET_StgFunInfoExtraRev_arity = 0
  , GHC.Settings.pc_OFFSET_stgGCEnter1 = 0
  , GHC.Settings.pc_OFFSET_stgGCFun = 0
  , GHC.Settings.pc_OFFSET_StgHeader_ccs = 0
  , GHC.Settings.pc_OFFSET_StgHeader_ldvw = 0
  , GHC.Settings.pc_OFFSET_StgMutArrPtrs_ptrs = 0
  , GHC.Settings.pc_OFFSET_StgMutArrPtrs_size = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rCCCS = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rCurrentNursery = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rCurrentTSO = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rD1 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rD2 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rD3 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rD4 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rD5 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rD6 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rF1 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rF2 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rF3 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rF4 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rF5 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rF6 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rHp = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rHpAlloc = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rHpLim = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rL1 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rR1 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rR10 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rR2 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rR3 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rR4 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rR5 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rR6 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rR7 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rR8 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rR9 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rSp = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rSpLim = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rXMM1 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rXMM2 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rXMM3 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rXMM4 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rXMM5 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rXMM6 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rYMM1 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rYMM2 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rYMM3 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rYMM4 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rYMM5 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rYMM6 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rZMM1 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rZMM2 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rZMM3 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rZMM4 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rZMM5 = 0
  , GHC.Settings.pc_OFFSET_StgRegTable_rZMM6 = 0
  , GHC.Settings.pc_OFFSET_StgSmallMutArrPtrs_ptrs = 0
  , GHC.Settings.pc_OFFSET_StgStack_sp = 0
  , GHC.Settings.pc_OFFSET_StgStack_stack = 0
  , GHC.Settings.pc_OFFSET_StgTSO_alloc_limit = 0
  , GHC.Settings.pc_OFFSET_StgTSO_cccs = 0
  , GHC.Settings.pc_OFFSET_StgTSO_stackobj = 0
  , GHC.Settings.pc_OFFSET_StgUpdateFrame_updatee = 0
  , GHC.Settings.pc_PROF_HDR_SIZE = 0
  , GHC.Settings.pc_REP_CostCentreStack_mem_alloc = 0
  , GHC.Settings.pc_REP_CostCentreStack_scc_count = 0
  , GHC.Settings.pc_REP_StgEntCounter_allocd = 0
  , GHC.Settings.pc_REP_StgEntCounter_allocs = 0
  , GHC.Settings.pc_REP_StgFunInfoExtraFwd_arity = 0
  , GHC.Settings.pc_REP_StgFunInfoExtraRev_arity = 0
  , GHC.Settings.pc_RESERVED_C_STACK_BYTES = 0
  , GHC.Settings.pc_RESERVED_STACK_WORDS = 0
  , GHC.Settings.pc_SIZEOF_CostCentreStack = 0
  , GHC.Settings.pc_SIZEOF_StgArrBytes_NoHdr = 0
  , GHC.Settings.pc_SIZEOF_StgFunInfoExtraRev = 0
  , GHC.Settings.pc_SIZEOF_StgMutArrPtrs_NoHdr = 0
  , GHC.Settings.pc_SIZEOF_StgSmallMutArrPtrs_NoHdr = 0
  , GHC.Settings.pc_SIZEOF_StgSMPThunkHeader = 0
  , GHC.Settings.pc_SIZEOF_StgUpdateFrame_NoHdr = 0
  , GHC.Settings.pc_STD_HDR_SIZE = 0
  , GHC.Settings.pc_TAG_BITS = 0
  , GHC.Settings.pc_TICKY_BIN_COUNT = 0
  , GHC.Settings.pc_WORD_SIZE = 0
  }

initialPlatformMini :: GHC.Settings.PlatformMini
initialPlatformMini = GHC.Settings.PlatformMini
  { GHC.Settings.platformMini_arch = GHC.Platform.ArchX86_64
  , GHC.Settings.platformMini_os = GHC.Platform.OSLinux
  }

initialTargetPlatform :: GHC.Settings.Platform
initialTargetPlatform = GHC.Settings.Platform
  { GHC.Settings.platformByteOrder = GHC.ByteOrder.LittleEndian
  , GHC.Settings.platformHasGnuNonexecStack = False
  , GHC.Settings.platformHasIdentDirective = False
  , GHC.Settings.platformHasSubsectionsViaSymbols = False
  , GHC.Settings.platformIsCrossCompiling = False
  , GHC.Settings.platformLeadingUnderscore = False
  , GHC.Settings.platformMini = initialPlatformMini
  , GHC.Settings.platformTablesNextToCode = False
  , GHC.Settings.platformUnregisterised = False
  , GHC.Settings.platformWordSize = GHC.Platform.PW8
  }

initialToolSettings :: GHC.Settings.ToolSettings
initialToolSettings = GHC.Settings.ToolSettings
  { GHC.Settings.toolSettings_ccSupportsNoPie = False
  , GHC.Settings.toolSettings_extraGccViaCFlags = []
  , GHC.Settings.toolSettings_ldIsGnuLd = False
  , GHC.Settings.toolSettings_ldSupportsBuildId = False
  , GHC.Settings.toolSettings_ldSupportsCompactUnwind = False
  , GHC.Settings.toolSettings_ldSupportsFilelist = False
  , GHC.Settings.toolSettings_opt_a = []
  , GHC.Settings.toolSettings_opt_c = []
  , GHC.Settings.toolSettings_opt_cxx = []
  , GHC.Settings.toolSettings_opt_F = []
  , GHC.Settings.toolSettings_opt_i = []
  , GHC.Settings.toolSettings_opt_l = []
  , GHC.Settings.toolSettings_opt_L = []
  , GHC.Settings.toolSettings_opt_lc = []
  , GHC.Settings.toolSettings_opt_lcc = []
  , GHC.Settings.toolSettings_opt_lm = []
  , GHC.Settings.toolSettings_opt_lo = []
  , GHC.Settings.toolSettings_opt_P = []
  , GHC.Settings.toolSettings_opt_P_fingerprint =
    GHC.Utils.Fingerprint.fingerprint0
  , GHC.Settings.toolSettings_opt_windres = []
  , GHC.Settings.toolSettings_pgm_a = ("", [])
  , GHC.Settings.toolSettings_pgm_ar = ""
  , GHC.Settings.toolSettings_pgm_c = ""
  , GHC.Settings.toolSettings_pgm_dll = ("", [])
  , GHC.Settings.toolSettings_pgm_F = ""
  , GHC.Settings.toolSettings_pgm_i = ""
  , GHC.Settings.toolSettings_pgm_install_name_tool = ""
  , GHC.Settings.toolSettings_pgm_l = ("", [])
  , GHC.Settings.toolSettings_pgm_L = ""
  , GHC.Settings.toolSettings_pgm_lc = ("", [])
  , GHC.Settings.toolSettings_pgm_lcc = ("", [])
  , GHC.Settings.toolSettings_pgm_libtool = ""
  , GHC.Settings.toolSettings_pgm_lm = ("", [])
  , GHC.Settings.toolSettings_pgm_lo = ("", [])
  , GHC.Settings.toolSettings_pgm_otool = ""
  , GHC.Settings.toolSettings_pgm_P = ("", [])
  , GHC.Settings.toolSettings_pgm_ranlib = ""
  , GHC.Settings.toolSettings_pgm_T = ""
  , GHC.Settings.toolSettings_pgm_windres = ""
  }
