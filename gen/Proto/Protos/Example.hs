{- This file was auto-generated from protos/example.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  UndecidableInstances, GeneralizedNewtypeDeriving,
  MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
  PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports#-}
module Proto.Protos.Example
       (Example(..), HelloReq(), HelloRsp(), LargeArrayReq(),
        LargeArrayRsp())
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
{- | Fields :

 -}
data LargeArrayReq = LargeArrayReq{_LargeArrayReq'_unknownFields ::
                                   !Data.ProtoLens.FieldSet}
                       deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show LargeArrayReq where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message LargeArrayReq where
        messageName _ = Data.Text.pack "example.LargeArrayReq"
        fieldsByTag = let in Data.Map.fromList []
        unknownFields
          = Lens.Family2.Unchecked.lens _LargeArrayReq'_unknownFields
              (\ x__ y__ -> x__{_LargeArrayReq'_unknownFields = y__})
        defMessage = LargeArrayReq{_LargeArrayReq'_unknownFields = ([])}
instance Control.DeepSeq.NFData LargeArrayReq where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_LargeArrayReq'_unknownFields x__) (())
{- | Fields :

    * 'Proto.Protos.Example_Fields.vals' @:: Lens' LargeArrayRsp [Data.Int.Int32]@
 -}
data LargeArrayRsp = LargeArrayRsp{_LargeArrayRsp'vals ::
                                   ![Data.Int.Int32],
                                   _LargeArrayRsp'_unknownFields :: !Data.ProtoLens.FieldSet}
                       deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show LargeArrayRsp where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' LargeArrayRsp "vals"
           ([Data.Int.Int32])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _LargeArrayRsp'vals
                 (\ x__ y__ -> x__{_LargeArrayRsp'vals = y__}))
              Prelude.id
instance Data.ProtoLens.Message LargeArrayRsp where
        messageName _ = Data.Text.pack "example.LargeArrayRsp"
        fieldsByTag
          = let vals__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "vals"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Packed
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "vals")))
                      :: Data.ProtoLens.FieldDescriptor LargeArrayRsp
              in
              Data.Map.fromList [(Data.ProtoLens.Tag 1, vals__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _LargeArrayRsp'_unknownFields
              (\ x__ y__ -> x__{_LargeArrayRsp'_unknownFields = y__})
        defMessage
          = LargeArrayRsp{_LargeArrayRsp'vals = [],
                          _LargeArrayRsp'_unknownFields = ([])}
instance Control.DeepSeq.NFData LargeArrayRsp where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_LargeArrayRsp'_unknownFields x__)
                (Control.DeepSeq.deepseq (_LargeArrayRsp'vals x__) (()))
data Example = Example{}
                 deriving ()
instance Data.ProtoLens.Service.Types.Service Example where
        type ServiceName Example = "Example"
        type ServicePackage Example = "example"
        type ServiceMethods Example = '["hello", "largeArray"]
instance Data.ProtoLens.Service.Types.HasMethodImpl Example "hello"
         where
        type MethodName Example "hello" = "Hello"
        type MethodInput Example "hello" = HelloReq
        type MethodOutput Example "hello" = HelloRsp
        type MethodStreamingType Example "hello" =
             'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Example
           "largeArray"
         where
        type MethodName Example "largeArray" = "LargeArray"
        type MethodInput Example "largeArray" = LargeArrayReq
        type MethodOutput Example "largeArray" = LargeArrayRsp
        type MethodStreamingType Example "largeArray" =
             'Data.ProtoLens.Service.Types.NonStreaming