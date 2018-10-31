{- This file was auto-generated from protos/example.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  UndecidableInstances, GeneralizedNewtypeDeriving,
  MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
  PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports#-}
module Proto.Protos.Example (Example(..), HelloReq(), HelloRsp())
       where
import qualified Data.ProtoLens.Runtime.Control.DeepSeq
       as Control.DeepSeq
import qualified Data.ProtoLens.Runtime.Lens.Labels.Prism
       as Lens.Labels.Prism
import qualified Data.ProtoLens.Runtime.Prelude as Prelude
import qualified Data.ProtoLens.Runtime.Data.Int as Data.Int
import qualified Data.ProtoLens.Runtime.Data.Word as Data.Word
import qualified Data.ProtoLens.Runtime.Data.ProtoLens
       as Data.ProtoLens
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Message.Enum
       as Data.ProtoLens.Message.Enum
import qualified
       Data.ProtoLens.Runtime.Data.ProtoLens.Service.Types
       as Data.ProtoLens.Service.Types
import qualified Data.ProtoLens.Runtime.Lens.Family2
       as Lens.Family2
import qualified Data.ProtoLens.Runtime.Lens.Family2.Unchecked
       as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Runtime.Data.Text as Data.Text
import qualified Data.ProtoLens.Runtime.Data.Map as Data.Map
import qualified Data.ProtoLens.Runtime.Data.ByteString
       as Data.ByteString
import qualified Data.ProtoLens.Runtime.Data.ByteString.Char8
       as Data.ByteString.Char8
import qualified Data.ProtoLens.Runtime.Lens.Labels as Lens.Labels
import qualified Data.ProtoLens.Runtime.Text.Read as Text.Read

{- | Fields :

    * 'Proto.Protos.Example_Fields.whom' @:: Lens' HelloReq Data.Text.Text@
 -}
data HelloReq = HelloReq{_HelloReq'whom :: !Data.Text.Text,
                         _HelloReq'_unknownFields :: !Data.ProtoLens.FieldSet}
                  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show HelloReq where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' HelloReq "whom" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _HelloReq'whom
                 (\ x__ y__ -> x__{_HelloReq'whom = y__}))
              Prelude.id
instance Data.ProtoLens.Message HelloReq where
        messageName _ = Data.Text.pack "example.HelloReq"
        fieldsByTag
          = let whom__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "whom"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "whom")))
                      :: Data.ProtoLens.FieldDescriptor HelloReq
              in
              Data.Map.fromList [(Data.ProtoLens.Tag 1, whom__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _HelloReq'_unknownFields
              (\ x__ y__ -> x__{_HelloReq'_unknownFields = y__})
        defMessage
          = HelloReq{_HelloReq'whom = Data.ProtoLens.fieldDefault,
                     _HelloReq'_unknownFields = ([])}
instance Control.DeepSeq.NFData HelloReq where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_HelloReq'_unknownFields x__)
                (Control.DeepSeq.deepseq (_HelloReq'whom x__) (()))
{- | Fields :

    * 'Proto.Protos.Example_Fields.whom' @:: Lens' HelloRsp Data.Text.Text@
 -}
data HelloRsp = HelloRsp{_HelloRsp'whom :: !Data.Text.Text,
                         _HelloRsp'_unknownFields :: !Data.ProtoLens.FieldSet}
                  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show HelloRsp where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' HelloRsp "whom" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _HelloRsp'whom
                 (\ x__ y__ -> x__{_HelloRsp'whom = y__}))
              Prelude.id
instance Data.ProtoLens.Message HelloRsp where
        messageName _ = Data.Text.pack "example.HelloRsp"
        fieldsByTag
          = let whom__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "whom"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "whom")))
                      :: Data.ProtoLens.FieldDescriptor HelloRsp
              in
              Data.Map.fromList [(Data.ProtoLens.Tag 1, whom__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _HelloRsp'_unknownFields
              (\ x__ y__ -> x__{_HelloRsp'_unknownFields = y__})
        defMessage
          = HelloRsp{_HelloRsp'whom = Data.ProtoLens.fieldDefault,
                     _HelloRsp'_unknownFields = ([])}
instance Control.DeepSeq.NFData HelloRsp where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_HelloRsp'_unknownFields x__)
                (Control.DeepSeq.deepseq (_HelloRsp'whom x__) (()))
data Example = Example{}
                 deriving ()
instance Data.ProtoLens.Service.Types.Service Example where
        type ServiceName Example = "Example"
        type ServicePackage Example = "example"
        type ServiceMethods Example = '["hello"]
instance Data.ProtoLens.Service.Types.HasMethodImpl Example "hello"
         where
        type MethodName Example "hello" = "Hello"
        type MethodInput Example "hello" = HelloReq
        type MethodOutput Example "hello" = HelloRsp
        type MethodStreamingType Example "hello" =
             'Data.ProtoLens.Service.Types.NonStreaming