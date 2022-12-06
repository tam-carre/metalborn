{-# LANGUAGE OverloadedRecordDot #-}

-- | For documentation on this module, please read `../../docs/DB.md`
-- | TODO: Simplify the DB module. TH + records ?
module App.DB (createCharacter, deleteCharacter, getCharacter) where

import App                             (App, AppError (..), DBError (..), intercept)
import App.Character                   (Character (..))
import App.Character.Abilities         (Abilities (..), AbilitiesObtained (..))
import App.Character.Description       (DescriptionBlock (..))
import App.Character.Metalborn         (Ferring, Halfborn (..), Metal, Metalborn (..), Misting,
                                        Singleborn (..), Twinborn)
import App.Character.Name              (Name (..))
import App.Gender                      (Gender (..))
import Control.Lens                    (_1, _2, (^.))
import Data.Generics.Product.Positions (HasPosition (..))
import Data.Profunctor.Product         (p11, p5)
import Data.Profunctor.Product.Default (Default)
import Data.Profunctor.Product.Default qualified as D
import Database.PostgreSQL.Simple      qualified as PGS
import Opaleye                         (DefaultFromField (..), Delete (..), Field, FieldNullable,
                                        FromField, Insert (..), IsSqlType (..), Select, SqlArray,
                                        SqlBool, SqlInt4, SqlText, SqlVarcharN, Table, ToFields,
                                        rCount, runDelete, runInsert, runSelectI, selectTable,
                                        sqlArray, sqlString, table, tableField, toFields,
                                        toToFields, unsafeCast, unsafeCoerceField, where_, (.&&),
                                        (.==))
import Opaleye                         qualified as O
import Opaleye.Experimental.Enum       (EnumMapper (..), enumMapper)
import Opaleye.Internal.Inferrable     (Inferrable (..))

----------------------------------------------------------------------------------------------------
-- Characters (exported)

getCharacter ∷ Name → Gender → App (Maybe Character)
getCharacter name gender = do
  rows ← toApp runSelectI (characterWithDescSelect name gender)
  pure $ toCharacter rows
  where
  toCharacter = \case
    []              → Nothing
    rows@((x, _):_) → Just $ Character name gender (rowToAbils x) (mkBlock <$> rows)
  mkBlock (_,(ctor, content)) = ctor content
  rowToAbils (_, _, fb, hb, tb, m, f, spkA, spkF, mdlF, gren) =
    Abilities mb obt
    where
    mb = case (fb, hb, tb, m, f) of
      (True, _, _, _, _)                → Just Fullborn
      (_, Just SqlMistborn, _, _, _)    → Just . Halfborn $ Mistborn f
      (_, Just SqlFeruchemist, _, _, _) → Just . Halfborn $ Feruchemist m
      (_, _, _, Just mi, Just fe)       → Just $ Twinborn mi fe tb
      (_, _, _, Just mi, Nothing)       → Just . Singleborn $ Misting mi
      (_, _, _, Nothing, Just fe)       → Just . Singleborn $ Ferring fe
      _                                 → Nothing
    obt = AbilitiesObtained spkA spkF mdlF gren

createCharacter ∷ Character → App ()
createCharacter (Character name gender abilities descriptionBlocks) =
  traverse_ (toApp runInsert)
    [ characterInsert name gender abilities
    , descriptionBlockInsert name gender descriptionBlocks
    ]

deleteCharacter ∷ Name → Gender → App ()
deleteCharacter name gender =
  traverse_ (toApp runDelete)
    [ characterDelete name gender
    , descriptionBlockDelete name gender
    ]

----------------------------------------------------------------------------------------------------
-- Utils (not exported)

type F  a = Field a
type F_ a = FieldNullable a

toApp ∷ (PGS.Connection → a → IO b) → a → App b
toApp f x =
  ( liftIO . (`f` x) =≪ asks (^. #db)
  ) `intercept` \err → DBError $
    case decodeUtf8 @String (PGS.sqlState err) of
      "23505" → UniqueConstraintViolated
      _       → OtherDBError err

----------------------------------------------------------------------------------------------------
-- Characters (not exported)

type CharactersRow =
  ( F  SqlVarcharN
  , F  SqlGender
  , F  SqlBool
  , F_ SqlHalfborn
  , F_ SqlTwinborn
  , F_ SqlMisting
  , F_ SqlFerring
  , F  (SqlArray SqlMetal)
  , F  (SqlArray SqlMetal)
  , F  (SqlArray SqlMetal)
  , F  SqlBool
  )

charactersTable ∷ Table CharactersRow CharactersRow
charactersTable = table "characters" $ p11
  ( tableField "name"
  , tableField "gender"
  , tableField "fullborn"
  , tableField "halfborn"
  , tableField "twinborn"
  , tableField "misting"
  , tableField "ferring"
  , tableField "spiked_a"
  , tableField "spiked_f"
  , tableField "medall_f"
  , tableField "grenade"
  )

characterWithDescSelect ∷ Name → Gender → Select (CharactersRow, DescriptionBlockRow)
characterWithDescSelect (Name (toFields → name)) (toFields → gender) = do
  row ← selectTable charactersTable
  let (rowName, rowGender) = (row ^. _1, row ^. _2)
  where_ (name .== rowName .&& gender .== rowGender)
  blocks ← descriptionBlocksForCharacter name gender
  pure (row, blocks)

characterInsert ∷ Name → Gender → Abilities → Insert Int64
characterInsert (Name (toFields → name)) (toFields → gender) (Abilities mb o) =
  Insert charactersTable [row] rCount Nothing
  where
  n = O.null
  (false, true, mistb, feru) =
    (toFields False, toFields True, toFields SqlMistborn, toFields SqlFeruchemist)
  mkRow fullborn halfborn twinborn misting ferring =
    ( name
    , gender
    , fullborn
    , halfborn
    , twinborn
    , misting
    , ferring
    , sqlArray toFields o.spikedA
    , sqlArray toFields o.spikedF
    , sqlArray toFields o.medallF
    , toFields o.grenade
    )
  row = case mb of
    Nothing → mkRow false n n n n
    Just pwr → case pwr of
      Fullborn                       → mkRow true n n n n
      Halfborn (Mistborn ferring)    → mkRow false mistb n n (toFields ferring)
      Halfborn (Feruchemist misting) → mkRow false feru n (toFields misting) n
      Twinborn m f t                 → mkRow false n (toFields t) (toFields m) (toFields f)
      Singleborn (Misting m)         → mkRow false n n (toFields m) n
      Singleborn (Ferring f)         → let x = toFields f in mkRow false n n n x

characterDelete ∷ Name → Gender → Delete Int64
characterDelete (Name (toFields → name)) (toFields → gender) =
  Delete charactersTable shouldDelete rCount where
    shouldDelete row = (row ^. _1) .== name
                   .&& (row ^. _2) .== gender

----------------------------------------------------------------------------------------------------
-- Description Block (not exported)

type DescriptionBlockInputOutput id
  = (id, F SqlDescriptionBlockCtor, F SqlText, F SqlVarcharN, F SqlGender)

type DescriptionBlockInput
  = DescriptionBlockInputOutput (Maybe (F SqlInt4))

type DescriptionBlockOutput
  = DescriptionBlockInputOutput (F SqlInt4)

descriptionBlocksTable ∷ Table DescriptionBlockInput DescriptionBlockOutput
descriptionBlocksTable = table "description_blocks" $ p5
  ( tableField "id"
  , tableField "kind"
  , tableField "content"
  , tableField "character_name"
  , tableField "character_gender"
  )

type DescriptionBlockRow = (F SqlDescriptionBlockCtor, F SqlText)

descriptionBlocksForCharacter ∷ F SqlVarcharN → F SqlGender → Select DescriptionBlockRow
descriptionBlocksForCharacter cName cGender = do
  (_id, content, kind, cName', cGender') ← selectTable descriptionBlocksTable
  where_ (cName' .== cName .&& cGender' .== cGender)
  pure (content, kind)

descriptionBlockInsert ∷ Name → Gender → [DescriptionBlock] → Insert Int64
descriptionBlockInsert (Name name) gender blocks =
  Insert descriptionBlocksTable (map toTuple blocks) rCount Nothing where
    toTuple block =
      ( Nothing
      , toFields (descriptionBlockCtor block)
      , toFields (block ^. position @1)
      , toFields name
      , toFields gender
      )

descriptionBlockDelete ∷ Name → Gender → Delete Int64
descriptionBlockDelete (Name (toFields → name)) (toFields → gender) =
  Delete descriptionBlocksTable shouldDelete rCount where
    shouldDelete (_,_,_, name', gender') = name'   .== name
                                       .&& gender' .== gender

----------------------------------------------------------------------------------------------------
-- Gender Enum (not exported)

data SqlGender

sqlGenderMapper ∷ EnumMapper SqlGender Gender
sqlGenderMapper = enumMapper "gender" readMaybe show

instance DefaultFromField SqlGender Gender where
  defaultFromField = enumFromField sqlGenderMapper
instance gender ~ Gender ⇒ Default (Inferrable FromField) SqlGender gender where
  def = Inferrable D.def
instance D.Default ToFields Gender (F SqlGender) where
  def = enumToFields sqlGenderMapper

----------------------------------------------------------------------------------------------------
-- Metal Enum (not exported)

data SqlMetal

sqlMetalMapper ∷ EnumMapper SqlMetal Metal
sqlMetalMapper = enumMapper "metal" readMaybe show

instance DefaultFromField SqlMetal Metal where
  defaultFromField = enumFromField sqlMetalMapper
instance metal ~ Metal ⇒ Default (Inferrable FromField) SqlMetal metal where
  def = Inferrable D.def
instance D.Default ToFields Metal (F SqlMetal) where
  def = enumToFields sqlMetalMapper
instance IsSqlType SqlMetal where
  showSqlType _ = "metal"

----------------------------------------------------------------------------------------------------
-- Halfborn Enum (not exported)

data SqlHalfborn
data SqlHalfbornEnum = SqlMistborn | SqlFeruchemist deriving (Eq, Generic, Read)

halfbornShow ∷ IsString a ⇒ SqlHalfbornEnum → a
halfbornShow SqlMistborn    = "Mistborn"
halfbornShow SqlFeruchemist = "Feruchemist"

halfbornReadMaybe ∷ (Eq a, IsString a) ⇒ a → Maybe SqlHalfbornEnum
halfbornReadMaybe "Mistborn"    = Just SqlMistborn
halfbornReadMaybe "Feruchemist" = Just SqlFeruchemist
halfbornReadMaybe _             = Nothing

sqlHalfbornMapper ∷ EnumMapper SqlHalfborn SqlHalfbornEnum
sqlHalfbornMapper = enumMapper "halfborn" halfbornReadMaybe halfbornShow

instance DefaultFromField SqlHalfborn SqlHalfbornEnum where
  defaultFromField = enumFromField sqlHalfbornMapper
instance halfborn ~ SqlHalfbornEnum ⇒ Default (Inferrable FromField) SqlHalfborn halfborn where
  def = Inferrable D.def
instance D.Default ToFields SqlHalfbornEnum (F SqlHalfborn) where
  def = enumToFields sqlHalfbornMapper
instance D.Default ToFields SqlHalfbornEnum (F_ SqlHalfborn) where
  def = toToFields (unsafeCast "halfborn" . unsafeCoerceField . sqlString . halfbornShow)


----------------------------------------------------------------------------------------------------
-- Twinborn Enum (not exported)

data SqlTwinborn

sqlTwinbornMapper ∷ EnumMapper SqlTwinborn Twinborn
sqlTwinbornMapper = enumMapper "twinborn" readMaybe show

instance DefaultFromField SqlTwinborn Twinborn where
  defaultFromField = enumFromField sqlTwinbornMapper
instance twinborn ~ Twinborn ⇒ Default (Inferrable FromField) SqlTwinborn twinborn where
  def = Inferrable D.def
instance D.Default ToFields Twinborn (F SqlTwinborn) where
  def = enumToFields sqlTwinbornMapper
instance D.Default ToFields Twinborn (F_ SqlTwinborn) where
  def = toToFields (unsafeCast "twinborn" . unsafeCoerceField . sqlString . show)

----------------------------------------------------------------------------------------------------
-- Misting Enum (not exported)

data SqlMisting

sqlMistingMapper ∷ EnumMapper SqlMisting Misting
sqlMistingMapper = enumMapper "misting" readMaybe show

instance DefaultFromField SqlMisting Misting where
  defaultFromField = enumFromField sqlMistingMapper
instance misting ~ Misting ⇒ Default (Inferrable FromField) SqlMisting misting where
  def = Inferrable D.def
instance D.Default ToFields Misting (F SqlMisting) where
  def = enumToFields sqlMistingMapper
instance D.Default ToFields Misting (F_ SqlMisting) where
  def = toToFields (unsafeCast "misting" . unsafeCoerceField . sqlString . show)

----------------------------------------------------------------------------------------------------
-- Ferring Enum (not exported)

data SqlFerring

sqlFerringMapper ∷ EnumMapper SqlFerring Ferring
sqlFerringMapper = enumMapper "ferring" readMaybe show

instance DefaultFromField SqlFerring Ferring where
  defaultFromField = enumFromField sqlFerringMapper
instance ferring ~ Ferring ⇒ Default (Inferrable FromField) SqlFerring ferring where
  def = Inferrable D.def
instance D.Default ToFields Ferring (F SqlFerring) where
  def = enumToFields sqlFerringMapper
instance D.Default ToFields Ferring (F_ SqlFerring) where
  def = toToFields (unsafeCast "ferring" . unsafeCoerceField . sqlString . show)

----------------------------------------------------------------------------------------------------
-- DescriptionBlock Enum (not exported)

data SqlDescriptionBlockCtor

type DescriptionBlockCtor = Text → DescriptionBlock

descriptionBlockCtor ∷ DescriptionBlock → DescriptionBlockCtor
descriptionBlockCtor (AllomancyBlock _) = AllomancyBlock
descriptionBlockCtor (FeruchemyBlock _) = FeruchemyBlock
descriptionBlockCtor (TwinbornBlock _)  = TwinbornBlock
descriptionBlockCtor (SpikesBlock _)    = SpikesBlock
descriptionBlockCtor (MedallionBlock _) = MedallionBlock
descriptionBlockCtor (GrenadeBlock _)   = GrenadeBlock

sqlDescriptionBlockMapper ∷ EnumMapper SqlDescriptionBlockCtor DescriptionBlockCtor
sqlDescriptionBlockMapper = enumMapper "description_block" fromStr toStr where
  fromStr "AllomancyBlock" = Just AllomancyBlock
  fromStr "FeruchemyBlock" = Just FeruchemyBlock
  fromStr "TwinbornBlock"  = Just TwinbornBlock
  fromStr "SpikesBlock"    = Just SpikesBlock
  fromStr "MedallionBlock" = Just MedallionBlock
  fromStr "GrenadeBlock"   = Just GrenadeBlock
  fromStr _                = Nothing
  toStr ctor = case ctor "" of
    AllomancyBlock _ → "AllomancyBlock"
    FeruchemyBlock _ → "FeruchemyBlock"
    TwinbornBlock  _ → "TwinbornBlock"
    SpikesBlock    _ → "SpikesBlock"
    MedallionBlock _ → "MedallionBlock"
    GrenadeBlock   _ → "GrenadeBlock"

instance DefaultFromField SqlDescriptionBlockCtor DescriptionBlockCtor where
  defaultFromField = enumFromField sqlDescriptionBlockMapper

instance description_block ~ DescriptionBlockCtor ⇒ Default (Inferrable FromField) SqlDescriptionBlockCtor description_block where
  def = Inferrable D.def

instance D.Default ToFields DescriptionBlockCtor (F SqlDescriptionBlockCtor) where
  def = enumToFields sqlDescriptionBlockMapper
