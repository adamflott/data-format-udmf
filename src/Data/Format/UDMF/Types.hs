module Data.Format.UDMF.Types where

import           Relude

-- base
import qualified Text.Show                      ( Show(..) )

-- Hackage
import           Prettyprinter


data Comment
    = CommentSingleLine Text
    | CommentMultiLine Text

instance Pretty Comment where
    pretty (CommentSingleLine c) = "//" <+> pretty c
    pretty (CommentMultiLine  c) = surround (vsep (fmap pretty (lines c))) "/*\n" "\n*/"


data DInt32 = DInt32 (Maybe Int64) Int64

instance Pretty DInt32 where
    pretty (DInt32 Nothing  d) = pretty d
    pretty (DInt32 (Just v) _) = pretty v

newtype DInt32ND = DInt32ND Int64

instance Pretty DInt32ND where
    pretty (DInt32ND i) = pretty i


newtype DDouble = DDouble Double
                deriving newtype (Num)

instance Pretty DDouble where
    pretty (DDouble d) = pretty d

newtype DStr = DStr Text
             deriving newtype (IsString)

instance Pretty DStr where
    pretty (DStr s) = surround (pretty s) "\"" "\""

data DBool (a :: Bool) = DBool (Maybe Bool) Bool
                       deriving stock (Generic)

instance Show (DBool a) where
    show (DBool b db) = show b <> " (default: " <> show db <> ")"

instance Pretty (DBool a) where
    pretty (DBool Nothing  d) = pb d
    pretty (DBool (Just v) _) = pb v


-- https://github.com/coelckers/gzdoom/blob/master/specs/udmf.txt

data NamespaceSpec = NSDoom | NSHeretic | NSHexen | NSStrife

instance Pretty NamespaceSpec where
    pretty = \case
        NSDoom    -> "doom"
        NSHeretic -> "heretic"
        NSHexen   -> "hexen"
        NSStrife  -> "strife"

data Namespace
    = NamespaceSpec NamespaceSpec
    | NamespaceZDoom
    | NamespaceExtended Text

instance Pretty Namespace where
    pretty ns =
        pretty ("namespace=\"" :: Text)
            <> case ns of
                   NamespaceSpec s     -> pretty s
                   NamespaceZDoom      -> pretty ("zdoom" :: Text)
                   NamespaceExtended t -> pretty t
            <> "\";"

data LineDef = LineDef {
      _lineDefId :: Maybe DInt32 -- ^ ID of line. Interpreted as tag or scripting id. Default = -1.

    , _lineDefV1 :: DInt32 -- ^ Index of first vertex. No valid default.
    , _lineDefV2 :: DInt32 -- ^ Index of second vertex. No valid default.

    , _lineDefBlocking       :: Maybe (DBool 'False) -- ^ true = line blocks things.
    , _lineDefBlockMonsters  :: Maybe (DBool 'False) -- ^ true = line blocks monsters.
    , _lineDefTwoSided       :: Maybe (DBool 'False) -- ^ true = line is 2S.
    , _lineDefDontPegTop     :: Maybe (DBool 'False) -- ^ true = upper texture unpegged.
    , _lineDefDontPegBottom  :: Maybe (DBool 'False) -- ^ true = lower texture unpegged.
    , _lineDefSecret         :: Maybe (DBool 'False) -- ^ true = drawn as 1S on map.
    , _lineDefBlockSound     :: Maybe (DBool 'False) -- ^ true = blocks sound.
    , _lineDefDontDraw       :: Maybe (DBool 'False) -- ^ true = line never drawn on map.
    , _lineDefMapped         :: Maybe (DBool 'False) -- ^ true = always appears on map.

    , _lineDefPassUse   :: Maybe (DBool 'False) -- ^ true = passes use action.

    , _lineDefPlayerCross    :: Maybe (DBool 'False) -- ^ true = player can cross.
    , _lineDefPlayerUse      :: Maybe (DBool 'False) -- ^ true = player can use.
    , _lineDefMonsterCross   :: Maybe (DBool 'False) -- ^ true = monster can cross.
    , _lineDefMonsterUse     :: Maybe (DBool 'False) -- ^ true = monster can use.
    , _lineDefImpact         :: Maybe (DBool 'False) -- ^ true = projectile can activate.
    , _lineDefPlayerPush     :: Maybe (DBool 'False) -- ^ true = player can push.
    , _lineDefMonsterPush    :: Maybe (DBool 'False) -- ^ true = monster can push.
    , _lineDefMissileCross   :: Maybe (DBool 'False) -- ^ true = projectile can cross.
    , _lineDefRepeatSpecial  :: Maybe (DBool 'False) -- ^ true = repeatable special.

    , _lineDefSpecial  :: Maybe DInt32 -- ^ Special. Default = 0.
    , _lineDefArg0     :: Maybe DInt32 -- ^ Argument 0. Default = 0.
    , _lineDefArg1     :: Maybe DInt32 -- ^ Argument 1. Default = 0.
    , _lineDefArg2     :: Maybe DInt32 -- ^ Argument 2. Default = 0.
    , _lineDefArg3     :: Maybe DInt32 -- ^ Argument 3. Default = 0.
    , _lineDefArg4     :: Maybe DInt32 -- ^ Argument 4. Default = 0.

    , _lineDefSideFront  :: DInt32ND     -- ^ Sidedef 1 index. No valid default.
    , _lineDefSideBack   :: Maybe DInt32 -- ^ Sidedef 2 index. Default = -1.

    , _lineDefComment :: Maybe DStr -- ^ A comment. Implementors should attach no special semantic meaning to this field.
    }

instance Pretty LineDef where
    pretty ld = prettyBlock
        "linedef"
        [ pe "id"            (_lineDefId ld)
        , pe "v1"            (Just (_lineDefV1 ld))
        , pe "v2"            (Just (_lineDefV2 ld))
        , pe "blocking"      (_lineDefBlocking ld)
        , pe "blockmonsters" (_lineDefBlockMonsters ld)
        , pe "twosided"      (_lineDefTwoSided ld)
        , pe "dontpegtop"    (_lineDefDontPegTop ld)
        , pe "dontpegbottom" (_lineDefDontPegBottom ld)
        , pe "secret"        (_lineDefSecret ld)
        , pe "blocksound"    (_lineDefBlockSound ld)
        , pe "dontdraw"      (_lineDefDontDraw ld)
        , pe "mapped"        (_lineDefMapped ld)
        , pe "passuse"       (_lineDefPassUse ld)
        , pe "playercross"   (_lineDefPlayerCross ld)
        , pe "playeruse"     (_lineDefPlayerUse ld)
        , pe "monstercrosss" (_lineDefMonsterCross ld)
        , pe "monsteruse"    (_lineDefMonsterUse ld)
        , pe "impact"        (_lineDefImpact ld)
        , pe "playerpush"    (_lineDefPlayerPush ld)
        , pe "monsterpush"   (_lineDefMonsterPush ld)
        , pe "missilecross"  (_lineDefMissileCross ld)
        , pe "repeatspecial" (_lineDefRepeatSpecial ld)
        , pe "special"       (_lineDefSpecial ld)
        , pe "arg0"          (_lineDefArg0 ld)
        , pe "arg1"          (_lineDefArg1 ld)
        , pe "arg2"          (_lineDefArg2 ld)
        , pe "arg3"          (_lineDefArg3 ld)
        , pe "arg4"          (_lineDefArg4 ld)
        , pe "sidefront"     (Just (_lineDefSideFront ld))
        , pe "sideback"      (_lineDefSideBack ld)
        , pe "comment"       (_lineDefComment ld)
        ]


{-
data LineDefStrife = LineDefStrife {
      // Strife specific flags. Support for other games is not defined by
      // default and these flags should be ignored when reading maps not for
      // the Strife namespace or maps for a port which supports these flags.

      translucent   = <bool>; // true = line is a Strife translucent line.
      jumpover      = <bool>; // true = line is a Strife railing.
      blockfloaters = <bool>; // true = line is a Strife float-blocker.

      // Note: SPAC flags should be set false in Doom/Heretic/Strife
      // namespace maps. Specials in those games do not support this
      // mechanism and instead imply activation parameters through the
      // special number. All flags default to false.
}
-}



{- |
   sidedef
   {
      offsetx = <integer>; // X Offset. Default = 0.
      offsety = <integer>; // Y Offset. Default = 0.

      texturetop    = <string>; // Upper texture. Default = "-".
      texturebottom = <string>; // Lower texture. Default = "-".
      texturemiddle = <string>; // Middle texture. Default = "-".

      sector = <integer>; // Sector index. No valid default.

      comment = <string>; // A comment. Implementors should attach no special
                          // semantic meaning to this field.
   }
-}
data SideDef = SideDef {
    _sideDefOffsetX         :: Maybe DInt32 -- ^ X Offset. Default = 0.
    , _sideDefOffsetY       :: Maybe DInt32 -- ^ Y Offset. Default = 0.
    , _sideDefTextureTop    :: Maybe DStr   -- ^ Upper texture. Default = "-".
    , _sideDefTextureBottom :: Maybe DStr   -- ^ Lower texture. Default = "-".
    , _sideDefTextureMiddle :: Maybe DStr   -- ^ Middle texture. Default = "-".
    , _sideDefSector        :: DInt32ND     -- ^ Sector index. No valid default.
    , _sideDefComment       :: Maybe DStr   -- ^ A comment. Implementors should attach no special semantic meaning to this field.
    }

instance Pretty SideDef where
    pretty sd = prettyBlock
        "sidedef"
        [ pe "offsetx"       (_sideDefOffsetX sd)
        , pe "offsety"       (_sideDefOffsetY sd)
        , pe "texturetop"    (_sideDefTextureTop sd)
        , pe "texturebottom" (_sideDefTextureBottom sd)
        , pe "texturemiddle" (_sideDefTextureMiddle sd)
        , pe "sector"        (Just (_sideDefSector sd))
        , pe "comment"       (_sideDefComment sd)
        ]


data Vertex = Vertex {
      _vertexX :: DDouble -- ^ X coordinate. No valid default.
    , _vertexY :: DDouble -- ^ Y coordinate. No valid default.
   }

instance Pretty Vertex where
    pretty vs = prettyBlock "vertex" [pe "x" (Just (_vertexX vs)), pe "y" (Just (_vertexY vs))]


data Sector = Sector {
     _sectorHeightFloor    :: Maybe DInt32 -- ^ Floor height. Default = 0.
    , _sectorHeightCeiling :: Maybe DInt32 -- ^ Ceiling height. Default = 0.

    , _sectorTextureFloor   :: DStr -- ^ Floor flat. No valid default.
    , _sectorTextureCeiling :: DStr -- ^ Ceiling flat. No valid default.

    , _sectorLightLevel :: Maybe DInt32 -- ^ Light level. Default = 160.

    , _sectorSpecial :: Maybe DInt32 -- ^ Sector special. Default = 0.
    , _sectorId      :: Maybe DInt32 -- ^ Sector tag/id. Default = 0.

    , _sectorComment :: Maybe DStr -- ^ A comment. Implementors should attach no special semantic meaning to this field.
   }


instance Pretty Sector where
    pretty se = prettyBlock
        "sector"
        [ pe "heightfloor"    (_sectorHeightFloor se)
        , pe "heightceiling"  (_sectorHeightCeiling se)
        , pe "texturefloor"   (Just (_sectorTextureFloor se))
        , pe "textureceiling" (Just (_sectorTextureCeiling se))
        , pe "lightlevel"     (_sectorLightLevel se)
        , pe "special"        (_sectorSpecial se)
        , pe "id"             (_sectorId se)
        , pe "comment"        (_sectorComment se)
        ]

data Thing = Thing {
      _thingId :: Maybe DInt32 -- ^ Thing ID. Default = 0.

    , _thingX :: DDouble -- ^ X coordinate. No valid default.
    , _thingY :: DDouble -- ^ Y coordinate. No valid default.

    , _thingHeight :: Maybe DDouble -- ^ Z height relative to floor. Default = 0.

    , _thingAngle :: Maybe DInt32 -- ^ Map angle of thing in degrees. Default = 0 (East).

    , _thingType :: DInt32ND -- ^ DoomedNum. No valid default.

    , _thingSkill1      :: Maybe (DBool 'False) -- ^ true = in skill 1.
    , _thingSkill2      :: Maybe (DBool 'False) -- ^ true = in skill 2.
    , _thingSkill3      :: Maybe (DBool 'False) -- ^ true = in skill 3.
    , _thingSkill4      :: Maybe (DBool 'False) -- ^ true = in skill 4.
    , _thingSkill5      :: Maybe (DBool 'False) -- ^ true = in skill 5.
    , _thingAmbush      :: Maybe (DBool 'False) -- ^ true = thing is deaf.
    , _thingSingle      :: Maybe (DBool 'False) -- ^ true = in SP mode.
    , _thingDM          :: Maybe (DBool 'False) -- ^ true = in DM mode.
    , _thingCoop        :: Maybe (DBool 'False) -- ^ true = in Coop.

    , _thingFriend      :: Maybe (DBool 'False) -- ^ true = MBF friend.

    , _thingComment :: Maybe DStr -- ^ A comment. Implementors should attach no special semantic meaning to this field.
   }

instance Pretty Thing where
    pretty t = prettyBlock
        "thing"
        [ pe "id"      (_thingId t)
        , pe "x"       (Just (_thingX t))
        , pe "y"       (Just (_thingY t))
        , pe "height"  (_thingHeight t)
        , pe "angle"   (_thingAngle t)
        , pe "type"    (Just (_thingType t))
        , pe "skill1"  (_thingSkill1 t)
        , pe "skill2"  (_thingSkill2 t)
        , pe "skill3"  (_thingSkill3 t)
        , pe "skill4"  (_thingSkill4 t)
        , pe "skill5"  (_thingSkill5 t)
        , pe "ambush"  (_thingAmbush t)
        , pe "single"  (_thingSingle t)
        , pe "dm"      (_thingDM t)
        , pe "coop"    (_thingCoop t)
        , pe "friend"  (_thingFriend t)
        , pe "comment" (_thingComment t)
        ]

{-

, // Hexen flags; not supported in Doom/Strife/Heretic namespaces.

, _thingdormant     = <bool>; // true = dormant thing.
, _thingclass1      = <bool>; // true = Present for pclass 1.
, _thingclass2      = <bool>; // true = Present for pclass 2.
, _thingclass3      = <bool>; // true = Present for pclass 3.


, // Strife specific flags. Support for other games is not defined by
, // default and these flags should be ignored when reading maps not for
, // the Strife namespace or maps for a port which supports these flags.
, _thingstanding    = <bool>; // true = Strife NPC flag.
, _thingstrifeally  = <bool>; // true = Strife ally flag.
, _thingtranslucent = <bool>; // true = Strife translucency flag.
, _thinginvisible   = <bool>; // true = Strife invisibility flag.

     // Note: suggested editor defaults for all skill, gamemode, and player
      // class flags is true rather than the UDMF default of false.

      // Thing special semantics are only defined for the Hexen namespace or
      // ports which implement this feature in their own namespace.

      special = <integer>; // Scripting special. Default = 0;
      arg0    = <integer>; // Argument 0. Default = 0.
      arg1    = <integer>; // Argument 1. Default = 0.
      arg2    = <integer>; // Argument 2. Default = 0.
      arg3    = <integer>; // Argument 3. Default = 0.
      arg4    = <integer>; // Argument 4. Default = 0.

-}


data UDMFI
    = UDMFComment Comment
    | UDMFNS Namespace
    | UDMFLineDef LineDef
    | UDMFSideDef SideDef
    | UDMFVertext Vertex
    | UDMFSector Sector
    | UDMFThing Thing

instance Pretty UDMFI where
    pretty = \case
        UDMFComment c  -> pretty c
        UDMFNS      ns -> pretty ns
        UDMFLineDef ld -> pretty ld
        UDMFSideDef sd -> pretty sd
        UDMFVertext v  -> pretty v
        UDMFSector  s  -> pretty s
        UDMFThing   t  -> pretty t

newtype UDMFV1_1 = UDMF [UDMFI]

instance Pretty UDMFV1_1 where
    pretty (UDMF us) = vsep (fmap pretty us)

--

newLineDef :: LineDef
newLineDef = LineDef { _lineDefId            = Nothing
                     , _lineDefV1            = DInt32 Nothing 0
                     , _lineDefV2            = DInt32 Nothing 0
                     , _lineDefBlocking      = Nothing
                     , _lineDefBlockMonsters = Nothing
                     , _lineDefTwoSided      = Nothing
                     , _lineDefDontPegTop    = Nothing
                     , _lineDefDontPegBottom = Nothing
                     , _lineDefSecret        = Nothing
                     , _lineDefBlockSound    = Nothing
                     , _lineDefDontDraw      = Nothing
                     , _lineDefMapped        = Nothing
                     , _lineDefPassUse       = Nothing
                     , _lineDefPlayerCross   = Nothing
                     , _lineDefPlayerUse     = Nothing
                     , _lineDefMonsterCross  = Nothing
                     , _lineDefMonsterUse    = Nothing
                     , _lineDefImpact        = Nothing
                     , _lineDefPlayerPush    = Nothing
                     , _lineDefMonsterPush   = Nothing
                     , _lineDefMissileCross  = Nothing
                     , _lineDefRepeatSpecial = Nothing
                     , _lineDefSpecial       = Nothing
                     , _lineDefArg0          = Nothing
                     , _lineDefArg1          = Nothing
                     , _lineDefArg2          = Nothing
                     , _lineDefArg3          = Nothing
                     , _lineDefArg4          = Nothing
                     , _lineDefSideFront     = DInt32ND 0
                     , _lineDefSideBack      = Nothing
                     , _lineDefComment       = Nothing
                     }


newSideDef :: SideDef
newSideDef = SideDef { _sideDefOffsetX       = Nothing
                     , _sideDefOffsetY       = Nothing
                     , _sideDefTextureTop    = Nothing
                     , _sideDefTextureBottom = Nothing
                     , _sideDefTextureMiddle = Nothing
                     , _sideDefSector        = DInt32ND 1
                     , _sideDefComment       = Nothing
                     }

newVertex :: (Double, Double) -> Vertex
newVertex (x, y) = Vertex { _vertexX = DDouble x, _vertexY = DDouble y }


newSector :: Sector
newSector = Sector { _sectorHeightFloor    = Nothing
                   , _sectorHeightCeiling  = Nothing
                   , _sectorTextureFloor   = DStr ""
                   , _sectorTextureCeiling = DStr ""
                   , _sectorLightLevel     = Nothing
                   , _sectorSpecial        = Nothing
                   , _sectorId             = Nothing
                   , _sectorComment        = Nothing
                   }


newThing :: DInt32ND -> Thing
newThing ty = Thing { _thingId      = Nothing
                    , _thingX       = DDouble 0.0
                    , _thingY       = DDouble 0.0
                    , _thingHeight  = Nothing
                    , _thingAngle   = Nothing
                    , _thingType    = ty
                    , _thingSkill1  = Just (DBool (Just True) False)
                    , _thingSkill2  = Just (DBool (Just True) False)
                    , _thingSkill3  = Just (DBool (Just True) False)
                    , _thingSkill4  = Just (DBool (Just True) False)
                    , _thingSkill5  = Just (DBool (Just True) False)
                    , _thingAmbush  = Nothing
                    , _thingSingle  = Just (DBool (Just True) False)
                    , _thingDM      = Just (DBool (Just True) False)
                    , _thingCoop    = Just (DBool (Just True) False)
                    , _thingFriend  = Nothing
                    , _thingComment = Nothing
                    }

--

renderUDMFV1_1 :: UDMFV1_1 -> Text
renderUDMFV1_1 = show . pretty

--

-- | Pretty Element - maybe pretty an element with an equals and ending with a semicolon, use with @@vsep (catMaybe [ pe "key" (Just "val") ])@@
pe :: Pretty a => Text -> Maybe a -> Maybe (Doc ann)
pe _ Nothing  = Nothing
pe k (Just v) = Just (pretty k <> equals <> pretty v <> semi)

prettyBlock :: Text -> [Maybe (Doc ann)] -> Doc ann
prettyBlock block_name elements = vsep [pretty block_name <+> lbrace, indent indentlevel (vsep (catMaybes elements)), rbrace]

-- | Pretty Bool
pb :: Bool -> Doc ann
pb = bool (pretty ("false" :: Text)) (pretty ("true" :: Text))

indentlevel :: Num n => n
indentlevel = 4
