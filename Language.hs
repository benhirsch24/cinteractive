{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module Language where
import Language.C
import Language.C.System.GCC
import Data.Aeson
import Data.Text (Text, pack, unpack)
import qualified Data.ByteString.UTF8 as BS

{--
 - type CTranslUnit = CTranslationUnit NodeInfo
 - data CTranslationUnit a = CTranslUnit [CExternalDeclaration a] a
 -}
instance ToJSON CTranslUnit where
   toJSON (CTranslUnit edecls _) = object ["node" .= pack "CTranslUnit", "decls" .= map toJSON edecls]

{--
 - type CExtDecl = CExternalDeclaration NodeInfo
 - data CExternalDeclaration a = 
 -   CDeclExt (CDeclaration a)
 - | CFDefExt (CFunctionDef a)
 - | CAsmExt (CStringLiteral a) a
 -}
instance ToJSON CExtDecl where
   toJSON (CDeclExt decl) = toJSON decl
   toJSON (CFDefExt fund) = toJSON fund
   toJSON (CAsmExt asmStmt _) = object ["node" .= pack "CAsmExt", "statement" .= pshow asmStmt]

{--
 - type CFunDef = CFunctionDef NodeInfo
 - data CFunctionDef a = CFunDef [CDeclarationSpecifier a] (CDeclarator a) [CDeclaration a] (CStatement a)
 -
 - specifiers can be extern or static
 - declarator must have declared function type, return type can be void or object other than array
 - decllist is for old style fundefs?
 - stmt is a compound statement (block) for function as AST ie Free Monad!
 -}
instance ToJSON CFunDef where
   toJSON (CFunDef declspecs declr decls stat _) = object ["node" .= pack "CFunDef", "fun_def" .= toJSON declr, "declarations" .= map toJSON decls, "statements" .= toJSON stat]

{--
 - CStatements and Exprs are both ASTs, TODO: not sure what the difference is
 - NOTE: CExpr -- no next?
 -}
instance ToJSON CStat where
   toJSON (CLabel ident stat attrs _) = object ["node" .= pack "CLabel", "ident" .= pshow ident, "next" .= toJSON stat]
   toJSON (CCase expr stat _) = object ["node" .= pack "CCase", "expr" .= toJSON expr, "next" .= toJSON stat]
   toJSON (CCases expr1 expr2 stat _) = object ["node" .= pack "CCases", "expr1" .= toJSON expr1, "expr2" .= toJSON expr2, "next" .= toJSON stat]
   toJSON (CDefault stat _) = object ["node" .= pack "CDefault", "next" .= toJSON stat]
   toJSON (CExpr expr _) = object ["node" .= pack "CExpr", "expr" .= toJSON expr]
   toJSON (CCompound idents items _) = object ["node" .= pack "CCompound", "idents" .= map (pack . show) idents, "block_items" .= map toJSON items]
   toJSON (CIf expr stif stelse _) = object ["node" .= pack "CIf", "guard" .= toJSON expr, "true" .= toJSON stif, "false" .= toJSON stelse]
   toJSON (CSwitch expr stat _) = object ["node" .= pack "CSwitch", "expr" .= toJSON expr, "next" .= toJSON stat]
   toJSON (CWhile guard stat dowhile _) = object ["node" .= pack "CWhile", "guard" .= toJSON guard, "next" .= toJSON stat]
   toJSON (CFor init guard step stat _) = object ["node" .= pack "CFor", "init" .= initls, "guard" .= toJSON guard, "step" .= toJSON step, "next" .= stat]
      where
      initls = case init of
         Right expr -> toJSON expr
         Left decl -> toJSON decl
   toJSON (CGoto ident _) = object ["node" .= pack "CGoto", "label" .= pshow ident]
   toJSON (CGotoPtr expr _) = object ["node" .= pack "CGotoPtr", "labelExpr" .= toJSON expr]
   toJSON (CCont _) = object ["node" .= pack "CContinue"]
   toJSON (CBreak _) = object ["node" .= pack "CBreak"]
   toJSON (CReturn expr _) = object ["node" .= pack "CReturn", "expr" .= toJSON expr]
   toJSON (CAsm _ _) = object ["node" .= pack "CAsm"]

instance ToJSON CBlockItem where
   toJSON (CBlockStmt stat) = toJSON stat
   toJSON (CBlockDecl decl) = toJSON decl
   toJSON (CNestedFunDef def) = toJSON def

instance ToJSON CInit where
   toJSON (CInitExpr expr _) = object ["node" .= pack "CInit", "assignment" .= toJSON expr]
   toJSON (CInitList init_list _) = object ["node" .= pack "CInitList", "list" .= map toJSON init_list]

instance ToJSON CDesignator where
   toJSON (CArrDesig expr _) = object ["node" .= pack "CArrDesig", "expr" .= toJSON expr]
   toJSON (CMemberDesig ident _) = object ["node" .= pack "CMemberDesig", "ident" .= pshow ident]
   toJSON (CRangeDesig expr1 expr2 _) = object ["node" .= pack "CRangeDesig", "from" .= toJSON expr1, "to" .= toJSON expr2]

instance ToJSON CDecl where
   toJSON (CDecl specs decls _) = object ["node" .= pack "CDecl", "specifiers" .= map toJSON specs, "declarations" .= map ppDecl decls]
      where
      ppDecl (decltr, initzr, expr) = object ["node" .= pack "DECL", "declarator" .= declhuh, "initializer" .= inithuh, "expr" .= exprhuh]
         where
         declhuh = case decltr of
                     Just decltr -> toJSON decltr
                     Nothing     -> toJSON (Nothing :: Maybe CDeclr)
         inithuh = case initzr of
                     Nothing -> toJSON (Nothing :: Maybe CInit)
                     Just initzr -> toJSON initzr
         exprhuh = case expr of
                     Nothing -> toJSON (Nothing :: Maybe CExpr)
                     Just expr -> toJSON expr

instance ToJSON CDeclr where
   toJSON (CDeclr name indirections asmname cattrs _) = object ["node" .= pack "CDeclr", "name" .= maybe "anonymous" show name, "attrs" .= map toJSON indirections]

instance ToJSON CDerivedDeclr where
   toJSON (CPtrDeclr quals _) = object ["node" .= pack "CPtrDeclr", "qualifiers" .= map toJSON quals]
   toJSON (CArrDeclr quals sz _) = object ["node" .= pack "CArrDeclr", "qualifiers" .= map toJSON quals, "size" .= toJSON sz]
   toJSON (CFunDeclr params attrs _) = object $ ["node" .= pack "CFunDeclr"] ++ funParams params ++ ["attrs" .= toJSON attrs]
      where
      funParams (Right (decls, isVariadic)) = ["params" .= map toJSON decls, "isVariadic" .= pshow isVariadic]
      funParams (Left names) = ["params" .= map pshow names, "isVariadic" .= pack "False"]

instance ToJSON CArrSize where
   toJSON (CNoArrSize isCompleteType) = object ["node" .= pack "CNoArrSize", "isCompleteType" .= pshow isCompleteType]
   toJSON (CArrSize isStatic expr) = object ["node" .= pack "CArrSize", "size" .= toJSON expr, "isStatic" .= pshow isStatic]

instance ToJSON CDeclSpec where
   toJSON (CStorageSpec spec) = object ["node" .= pack "CStorageSpec", "spec" .= toJSON spec]
   toJSON (CTypeSpec spec) = object ["node" .= pack "CTypeSpec", "spec" .= toJSON spec]
   toJSON (CTypeQual qual) = object ["node" .= pack "CTypeQual", "qual" .= toJSON qual]

instance ToJSON CStorageSpec where
   toJSON (CAuto _) = object ["node" .= pack "CAuto"]
   toJSON (CRegister _) = object ["node" .= pack "CRegister"]
   toJSON (CStatic _) = object ["node" .= pack "CStatic"]
   toJSON (CExtern _) = object ["node" .= pack "CExtern"]
   toJSON (CTypedef _) = object ["node" .= pack "CTypedef"]
   toJSON (CThread _) = object ["node" .= pack "CThread"]

instance ToJSON CTypeSpec where
   toJSON (CVoidType _) = object ["node" .= pack "CVoidType"]
   toJSON (CCharType _) = object ["node" .= pack "CCharType"]
   toJSON (CShortType _) = object ["node" .= pack "CShortType"]
   toJSON (CIntType _) = object ["node" .= pack "CIntType"]
   toJSON (CLongType _) = object ["node" .= pack "CLongType"]
   toJSON (CFloatType _) = object ["node" .= pack "CFloatType"]
   toJSON (CDoubleType _) = object ["node" .= pack "CDoubleType"]
   toJSON (CSignedType _) = object ["node" .= pack "CSignedType"]
   toJSON (CUnsigType _) = object ["node" .= pack "CUnsigType"]
   toJSON (CBoolType _) = object ["node" .= pack "CBoolType"]
   toJSON (CComplexType _) = object ["node" .= pack "CComplexType"]
   toJSON (CSUType struct_or_union _) = object ["node" .= pack "CSUType", "sutype" .= toJSON struct_or_union]
   toJSON (CEnumType enum _) = object ["node" .= pack "CEnumType", "enum" .= toJSON enum]
   toJSON (CTypeDef name _) = object ["node" .= pack "CTypeDef", "name" .= pshow name]
   toJSON (CTypeOfExpr expr _) = object ["node" .= pack "CTypeOfExpr", "expr" .= toJSON expr]
   toJSON (CTypeOfType tipe _) = object ["node" .= pack "CTypeOfType", "type" .= toJSON tipe]

instance ToJSON CStructUnion where
   toJSON (CStruct CStructTag ident fields attrs _) = object $ ["node" .= pack "CStruct", "ident" .= pshow ident] ++ maybeJSON fields
   toJSON (CStruct CUnionTag ident fields attrs _) = object $ ["node" .= pack "CUnion", "ident" .= pshow ident] ++ maybeJSON fields

maybeJSON Nothing = ["fields" .= pack "Null"]
maybeJSON (Just decls) = ["fields" .= map toJSON decls]

{--
 - CEnum identifier enumerator-list attrs represent as enum specifier
 - 
 - Either the identifier or the enumerator-list (or both) have to be present.
 - If enumerator-list is present, it has to be non-empty.
 - The enumerator list is of the form (enumeration-constant, enumeration-value?), where the latter is an optional constant integral expression.
 - attrs is a list of __attribute__ annotations associated with the enumeration specifier
 -
 - CEnum (Maybe Ident) (Maybe [(Ident, Maybe (CExpression a))]) [CAttribute a] a  
 -
--}
instance ToJSON CEnum where
   toJSON (CEnum name enum_list attrs _) = object ["node" .= pack "CEnum", "name" .= pshow name, "enum-list" .= thelist enum_list, "attrs" .= map toJSON attrs]
      where
      thelist Nothing = ["Null"]
      thelist (Just elist) = map listToJSON elist
      listToJSON (name, val) = object ["enum" .= pshow name, "val" .= toJSON val]

instance ToJSON CTypeQual where
   toJSON (CConstQual _) = object ["node" .= pack "CConstQual"]
   toJSON (CVolatQual _) = object ["node" .= pack "CVolatQual"]
   toJSON (CRestrQual _) = object ["node" .= pack "CRestrQual"]
   toJSON (CInlineQual _) = object ["node" .= pack "CInlineQual"]
   toJSON (CAttrQual attr) = object ["node" .= pack "CAttrQual", "attr" .= toJSON attr]

instance ToJSON CAttr where
   toJSON (CAttr name params _) = object ["node" .= pack "CAttr", "name" .= pshow name, "params" .= map toJSON params]

instance ToJSON CBinaryOp where
   toJSON CMulOp = String $ pack "CMulOp"
   toJSON CDivOp = String $ pack "CDivOp"
   toJSON CRmdOp = String $ pack "CRmdOp"
   toJSON CAddOp = String $ pack "CAddOp"
   toJSON CSubOp = String $ pack "CSubOp"
   toJSON CShlOp = String $ pack "CShlOp"
   toJSON CShrOp = String $ pack "CShrOp"
   toJSON CLeOp  = String $ pack "CLeOp"
   toJSON CGrOp  = String $ pack "CGrOp"
   toJSON CLeqOp = String $ pack "CLeqOp"
   toJSON CGeqOp = String $ pack "CGeqOp"
   toJSON CEqOp  = String $ pack "CEqOp"
   toJSON CNeqOp = String $ pack "CNeqOp"
   toJSON CAndOp = String $ pack "CAndOp"
   toJSON CXorOp = String $ pack "CXorOp"
   toJSON COrOp  = String $ pack "COrOp"
   toJSON CLndOp = String $ pack "CLndOp"
   toJSON CLorOp = String $ pack "CLorOp"

instance ToJSON CUnaryOp where
   toJSON CPreIncOp  = String $ pack "CPreIncOp"
   toJSON CPreDecOp  = String $ pack "CPreDecOp"
   toJSON CPostIncOp = String $ pack "CPostIncOp"
   toJSON CPostDecOp = String $ pack "CPostDecOp"
   toJSON CAdrOp     = String $ pack "CAdrOp"
   toJSON CIndOp     = String $ pack "CIndOp"
   toJSON CPlusOp    = String $ pack "CPlusOp"
   toJSON CMinOp     = String $ pack "CMinOp"
   toJSON CCompOp    = String $ pack "CCompOp"
   toJSON CNegOp     = String $ pack "CNegOp"

instance ToJSON CAssignOp where
   toJSON CAssignOp = String $ pack "CAssignOp"
   toJSON CMulAssOp = String $ pack "CMulAssOp"
   toJSON CDivAssOp = String $ pack "CDivAssOp"
   toJSON CRmdAssOp = String $ pack "CRmdAssOp"
   toJSON CAddAssOp = String $ pack "CAddAssOp"
   toJSON CSubAssOp = String $ pack "CSubAssOp"
   toJSON CShlAssOp = String $ pack "CShlAssOp"
   toJSON CShrAssOp = String $ pack "CShrAssOp"
   toJSON CAndAssOp = String $ pack "CAndAssOp"
   toJSON CXorAssOp = String $ pack "CXorAssOp"
   toJSON COrAssOp =  String $ pack "COrAssOp"


instance ToJSON CExpr where
   toJSON (CComma exprs _) = object ["node" .= pack "CComma", "exprs" .= map toJSON exprs]-- comma expr list n >= 2
   toJSON (CAssign op lvalue rvalue _) = object ["node" .= pack "CAssign", "op" .= toJSON op, "lvalue" .= toJSON lvalue, "rvalue" .= toJSON rvalue]
   toJSON (CCond cond texpr fexpr _) = object ["node" .= pack "CCond", "cond" .= toJSON cond, "true" .= toJSON texpr, "false" .= toJSON fexpr]
   toJSON (CBinary op erand1 erand2 _) = object ["node" .= pack "CBinary", "op" .= toJSON op, "erand1" .= toJSON erand1, "erand2" .= toJSON erand2]
   toJSON (CCast decl expr _) = object ["node" .= pack "CCast", "type" .= toJSON decl, "expr" .= toJSON expr]
   toJSON (CUnary op expr _) = object ["node" .= pack "CUnary", "op" .= toJSON op, "expr" .= toJSON expr]
   toJSON (CSizeofExpr expr _) = object ["node" .= pack "CSizeofExpr", "expr" .= toJSON expr]
   toJSON (CSizeofType tipe _) = object ["node" .= pack "CSizeofType", "type" .= toJSON tipe]
   toJSON (CAlignofExpr expr _) = object ["node" .= pack "CAlignofExpr", "expr" .= toJSON expr]
   toJSON (CAlignofType tipe _) = object ["node" .= pack "CAlignofType", "type" .= toJSON tipe]
   toJSON (CComplexReal real _) = object ["node" .= pack "CComplexReal", "real" .= toJSON real]
   toJSON (CComplexImag imag _) = object ["node" .= pack "CComplexImag", "imag" .= toJSON imag]
   toJSON (CIndex array index _) = object ["node" .= pack "CIndex", "array" .= toJSON array, "index" .= toJSON index]
   toJSON (CCall fun args _) = object ["node" .= pack "CCall", "function" .= toJSON fun, "args" .= map toJSON args]
   toJSON (CMember struct name deref _) = object ["node" .= pack "CMember", "name" .= pshow name, "struct" .= toJSON struct, "deref?" .= toJSON deref]
   toJSON (CVar ident _) = object ["node" .= pack "CVar", "name" .= pshow ident]
   toJSON (CConst const) = object ["node" .= pack "CConst", "const" .= toJSON const]
   toJSON (CCompoundLit decl initl _) = object ["node" .= pack "CCompoundLit", "decl" .= toJSON decl, "init-list" .= map toJSON initl]
   toJSON (CStatExpr stat _) = toJSON stat
   toJSON (CLabAddrExpr label _) = object ["node" .= pack "CLabAddrExpr", "label" .= pshow label]
   toJSON (CBuiltinExpr thing) = object ["node" .= pack "CBuiltinExpr", "expr" .= toJSON thing]

instance ToJSON CBuiltin where
   toJSON (CBuiltinVaArg expr decl _) = object ["node" .= pack "CBuiltinVaArg", "expr" .= toJSON expr, "type" .= toJSON decl]
   toJSON (CBuiltinOffsetOf decl desigs _) = object ["node" .= pack "CBuiltinOffsetOf", "type" .= toJSON decl, "designator-list" .= map toJSON desigs]
   toJSON (CBuiltinTypesCompatible decl1 decl2 _) = object ["node" .= pack "CBuiltinTypesCompatible", "type1" .= toJSON decl1, "type2" .= toJSON decl2]

instance ToJSON CConst where --i
   toJSON (CIntConst int _) = object ["node" .= pack "CIntConst", "val" .= toJSON int]
   toJSON (CCharConst char _) = object ["node" .= pack "CCharConst", "val" .= toJSON char]
   toJSON (CFloatConst float _) = object ["node" .= pack "CFloatConst", "val" .= toJSON float]
   toJSON (CStrConst str _) = object ["node" .= pack "CStrConst", "val" .= toJSON str]

instance ToJSON CString where --i
   toJSON (CString str _) = object ["node" .= pack "CString", "string" .= pshow str]

instance ToJSON CStrLit where --i
   toJSON (CStrLit str _) = object ["node" .= pack "CStrLit", "string" .= toJSON str]

instance ToJSON CInteger where --i
   toJSON (CInteger int base (Flags flag)) = object ["node" .= pack "CInteger", "int" .= pshow int, "base" .= pshow base, "flags" .= pshow flag]

instance Show CIntRepr where --i
   show DecRepr = "d"
   show HexRepr = "x"
   show OctalRepr = "o"

instance ToJSON CChar where --i
   toJSON (CChar char _) = object ["node" .= pack "CChar", "char" .= pshow char]
   toJSON (CChars chars _) = object ["node" .= pack "CChars", "chars" .= pshow chars]

instance ToJSON CFloat where --i
   toJSON (CFloat float) = object ["node" .= pack "CFloat", "float ".= pshow float]

byteStringToAST :: BS.ByteString -> Either ParseError CTranslUnit
byteStringToAST bs = parseC bs (initPos "cfile.c")

partialAST :: BS.ByteString -> Either ParseError (CStat, [Name])
partialAST bs = execParser statementP bs (initPos "partial.c") builtinTypeNames newNameSupply

getAST file = do
   parse_result <- parseCFile (newGCC "gcc") Nothing [] file
   case parse_result of
      Right ast -> return ast
      Left err -> error $ show err

pshow :: Show a => a -> Text
pshow = pack . show
