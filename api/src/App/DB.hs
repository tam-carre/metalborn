module App.DB (createCharacter, deleteCharacter, getCharacter) where

import App                             (App, AppError (..), DBError (..), intercept)
import App.Character                   (Character (..))
import App.Character.Description       (DescriptionBlock (..))
import App.Character.Name              (Name (..))
import App.Gender                      (Gender (..))
import Control.Lens                    ((^.))
import Data.Generics.Product.Positions (HasPosition (..))
import Data.Profunctor.Product         (p2, p5)
import Data.Profunctor.Product.Default (Default)
import Data.Profunctor.Product.Default qualified as D
import Database.PostgreSQL.Simple      qualified as PGS
import Opaleye                         (DefaultFromField (..), Delete (..), Field, FromField,
                                        Insert (..), Select, SqlInt4, SqlText, SqlVarcharN, Table,
                                        ToFields, rCount, runDelete, runInsert, runSelectI,
                                        selectTable, table, tableField, toFields, where_, (.&&),
                                        (.==))
import Opaleye.Experimental.Enum       (EnumMapper (..), enumMapper)
import Opaleye.Internal.Inferrable     (Inferrable (..))

----------------------------------------------------------------------------------------------------
-- Characters (exported)

getCharacter ∷ Name → Gender → App (Maybe Character)
getCharacter name gender = typed <$> toApp runSelectI (characterWithDescSelect name gender)
  where
  typed []                 = Nothing
  typed (((n, g), b):rows) = Just $ Character (Name n) g (mkBlock <$> (b:map snd rows))
  mkBlock (kind, content)  = kind content

createCharacter ∷ Character → App ()
createCharacter (Character name gender descriptionBlocks) =
  traverse_ (toApp runInsert)
    [ characterInsert name gender
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

toApp ∷ (PGS.Connection → a → IO b) → a → App b
toApp f x =
  ( liftIO . (`f` x) =≪ asks (^. #db)
  ) `intercept` \err → DBError $
    case decodeUtf8 @String (PGS.sqlState err) of
      "23505" → UniqueConstraintViolated
      _       → OtherDBError err

----------------------------------------------------------------------------------------------------
-- Characters (not exported)

charactersTable ∷ Table (Field SqlVarcharN, Field SqlGender) (Field SqlVarcharN, Field SqlGender)
charactersTable = table "characters" $ p2
  ( tableField "name"
  , tableField "gender"
  )

characterWithDescSelect
  ∷ Name
  → Gender
  → Select
     ( (Field SqlVarcharN, Field SqlGender)
     , (Field SqlDescriptionBlockCtor, Field SqlText)
     )
characterWithDescSelect (Name (toFields → name)) (toFields → gender) = do
  row@(name', gender') ← selectTable charactersTable
  where_ (name' .== name' .&& gender' .== gender)
  blocks ← descriptionBlocksForCharacter name gender
  pure (row, blocks)

characterInsert ∷ Name → Gender → Insert Int64
characterInsert (Name (toFields → name)) (toFields → gender) =
  Insert charactersTable [(name, gender)] rCount Nothing

characterDelete ∷ Name → Gender → Delete Int64
characterDelete (Name (toFields → name)) (toFields → gender) =
  Delete charactersTable shouldDelete rCount where
    shouldDelete (name', gender') = name'   .== name
                                .&& gender' .== gender

----------------------------------------------------------------------------------------------------
-- Description Block (not exported)

type DescriptionBlockInputOutput id
  = (id, Field SqlDescriptionBlockCtor, Field SqlText, Field SqlVarcharN, Field SqlGender)

type DescriptionBlockInput
  = DescriptionBlockInputOutput (Maybe (Field SqlInt4))

type DescriptionBlockOutput
  = DescriptionBlockInputOutput (Field SqlInt4)

descriptionBlocksTable ∷ Table DescriptionBlockInput DescriptionBlockOutput
descriptionBlocksTable = table "description_blocks" $ p5
  ( tableField "id"
  , tableField "kind"
  , tableField "content"
  , tableField "character_name"
  , tableField "character_gender"
  )

descriptionBlocksForCharacter
  ∷ Field SqlVarcharN
  → Field SqlGender
  → Select (Field SqlDescriptionBlockCtor, Field SqlText)
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
sqlGenderMapper = enumMapper "gender" fromStr show where
  fromStr "Male"   = Just Male
  fromStr "Female" = Just Female
  fromStr "Other"  = Just Other
  fromStr _        = Nothing

instance DefaultFromField SqlGender Gender where
  defaultFromField = enumFromField sqlGenderMapper

instance gender ~ Gender ⇒ Default (Inferrable FromField) SqlGender gender where
  def = Inferrable D.def

instance D.Default ToFields Gender (Field SqlGender) where
  def = enumToFields sqlGenderMapper

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

instance D.Default ToFields DescriptionBlockCtor (Field SqlDescriptionBlockCtor) where
  def = enumToFields sqlDescriptionBlockMapper
