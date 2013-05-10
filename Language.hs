{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module Language where
import Language.C
import Language.C.System.GCC
import Data.Aeson
import Data.Text (Text, pack, unpack)
import qualified Data.ByteString.UTF8 as BS

nodeLine :: NodeInfo -> Int
nodeLine = posRow . posOfNode

{--
 - type CTranslUnit = CTranslationUnit NodeInfo
 - data CTranslationUnit a = CTranslUnit [CExternalDeclaration a] a
 -}
instance ToJSON CTranslUnit where
   toJSON (CTranslUnit edecls inf) = object ["node" .= pack "CTranslUnit", "decls" .= map toJSON edecls, "line" .= nodeLine inf]

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
   toJSON (CFunDef declspecs declr decls stat inf) = object ["node" .= pack "CFunDef", "fun_def" .= toJSON declr, "declarations" .= map toJSON decls, "statements" .= toJSON stat, "line" .= nodeLine inf]

{--
 - CStatements and Exprs are both ASTs, TODO: not sure what the difference is
 - NOTE: CExpr -- no next?
 -}
instance ToJSON CStat where
   toJSON (CLabel ident stat attrs inf) = object ["node" .= pack "CLabel", "ident" .= pshow ident, "next" .= toJSON stat, "line" .= nodeLine inf]
   toJSON (CCase expr stat inf) = object ["node" .= pack "CCase", "expr" .= toJSON expr, "next" .= toJSON stat, "line" .= nodeLine inf]
   toJSON (CCases expr1 expr2 stat inf) = object ["node" .= pack "CCases", "expr1" .= toJSON expr1, "expr2" .= toJSON expr2, "next" .= toJSON stat, "line" .= nodeLine inf]
   toJSON (CDefault stat inf) = object ["node" .= pack "CDefault", "next" .= toJSON stat, "line" .= nodeLine inf]
   toJSON (CExpr expr inf) = object ["node" .= pack "CExpr", "expr" .= toJSON expr, "line" .= nodeLine inf]
   toJSON (CCompound labels items inf) = object ["node" .= pack "CCompound", "labels" .= map (pack . show) labels, "block_items" .= map toJSON items, "line" .= nodeLine inf]
   toJSON (CIf expr stif stelse inf) = object ["node" .= pack "CIf", "guard" .= toJSON expr, "true" .= toJSON stif, "false" .= toJSON stelse, "line" .= nodeLine inf]
   toJSON (CSwitch expr stat inf) = object ["node" .= pack "CSwitch", "expr" .= toJSON expr, "next" .= toJSON stat, "line" .= nodeLine inf]
   toJSON (CWhile guard stat dowhile inf) = object ["node" .= pack "CWhile", "guard" .= toJSON guard, "next" .= toJSON stat, "line" .= nodeLine inf]
   toJSON (CFor init guard step stat inf) = object ["node" .= pack "CFor", "init" .= initls, "guard" .= toJSON guard, "step" .= toJSON step, "next" .= stat, "line" .= nodeLine inf]
      where
      initls = case init of
         Right expr -> toJSON expr
         Left decl -> toJSON decl
   toJSON (CGoto ident inf) = object ["node" .= pack "CGoto", "label" .= pshow ident, "line" .= nodeLine inf]
   toJSON (CGotoPtr expr inf) = object ["node" .= pack "CGotoPtr", "labelExpr" .= toJSON expr, "line" .= nodeLine inf]
   toJSON (CCont inf) = object ["node" .= pack "CContinue", "line" .= nodeLine inf]
   toJSON (CBreak inf) = object ["node" .= pack "CBreak", "line" .= nodeLine inf]
   toJSON (CReturn expr inf) = object ["node" .= pack "CReturn", "expr" .= toJSON expr, "line" .= nodeLine inf]
   toJSON (CAsm _ _) = object ["node" .= pack "CAsm"]

instance ToJSON CBlockItem where
   toJSON (CBlockStmt stat) = toJSON stat
   toJSON (CBlockDecl decl) = toJSON decl
   toJSON (CNestedFunDef def) = toJSON def

instance ToJSON CInit where
   toJSON (CInitExpr expr inf) = object ["node" .= pack "CInit", "assignment" .= toJSON expr, "line" .= nodeLine inf]
   toJSON (CInitList init_list inf) = object ["node" .= pack "CInitList", "list" .= map toJSON init_list, "line" .= nodeLine inf]

instance ToJSON CDesignator where
   toJSON (CArrDesig expr inf) = object ["node" .= pack "CArrDesig", "expr" .= toJSON expr, "line" .= nodeLine inf]
   toJSON (CMemberDesig ident inf) = object ["node" .= pack "CMemberDesig", "ident" .= pshow ident, "line" .= nodeLine inf]
   toJSON (CRangeDesig expr1 expr2 inf) = object ["node" .= pack "CRangeDesig", "from" .= toJSON expr1, "to" .= toJSON expr2, "line" .= nodeLine inf]

instance ToJSON CDecl where
   toJSON (CDecl specs decls inf) = object ["node" .= pack "CDecl", "specifiers" .= map toJSON specs, "declarations" .= map ppDecl decls, "line" .= nodeLine inf]
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
   toJSON (CDeclr name indirections asmname cattrs inf) = object ["node" .= pack "CDeclr", "name" .= maybe "anonymous" show name, "attrs" .= map toJSON indirections, "line" .= nodeLine inf]

instance ToJSON CDerivedDeclr where
   toJSON (CPtrDeclr quals inf) = object ["node" .= pack "CPtrDeclr", "qualifiers" .= map toJSON quals, "line" .= nodeLine inf]
   toJSON (CArrDeclr quals sz inf) = object ["node" .= pack "CArrDeclr", "qualifiers" .= map toJSON quals, "size" .= toJSON sz, "line" .= nodeLine inf]
   toJSON (CFunDeclr params attrs inf) = object $ ["node" .= pack "CFunDeclr", "line" .= nodeLine inf] ++ funParams params ++ ["attrs" .= toJSON attrs]
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
   toJSON (CAuto inf) = object ["node" .= pack "CAuto", "line" .= nodeLine inf]
   toJSON (CRegister inf) = object ["node" .= pack "CRegister", "line" .= nodeLine inf]
   toJSON (CStatic inf) = object ["node" .= pack "CStatic", "line" .= nodeLine inf]
   toJSON (CExtern inf) = object ["node" .= pack "CExtern", "line" .= nodeLine inf]
   toJSON (CTypedef inf) = object ["node" .= pack "CTypedef", "line" .= nodeLine inf]
   toJSON (CThread inf) = object ["node" .= pack "CThread", "line" .= nodeLine inf]

instance ToJSON CTypeSpec where
   toJSON (CVoidType inf) = object ["node" .= pack "CVoidType", "line" .= nodeLine inf]
   toJSON (CCharType inf) = object ["node" .= pack "CCharType", "line" .= nodeLine inf]
   toJSON (CShortType inf) = object ["node" .= pack "CShortType", "line" .= nodeLine inf]
   toJSON (CIntType inf) = object ["node" .= pack "CIntType", "line" .= nodeLine inf]
   toJSON (CLongType inf) = object ["node" .= pack "CLongType", "line" .= nodeLine inf]
   toJSON (CFloatType inf) = object ["node" .= pack "CFloatType", "line" .= nodeLine inf]
   toJSON (CDoubleType inf) = object ["node" .= pack "CDoubleType", "line" .= nodeLine inf]
   toJSON (CSignedType inf) = object ["node" .= pack "CSignedType", "line" .= nodeLine inf]
   toJSON (CUnsigType inf) = object ["node" .= pack "CUnsigType", "line" .= nodeLine inf]
   toJSON (CBoolType inf) = object ["node" .= pack "CBoolType", "line" .= nodeLine inf]
   toJSON (CComplexType inf) = object ["node" .= pack "CComplexType", "line" .= nodeLine inf]
   toJSON (CSUType struct_or_union inf) = object ["node" .= pack "CSUType", "sutype" .= toJSON struct_or_union, "line" .= nodeLine inf]
   toJSON (CEnumType enum inf) = object ["node" .= pack "CEnumType", "enum" .= toJSON enum, "line" .= nodeLine inf]
   toJSON (CTypeDef name inf) = object ["node" .= pack "CTypeDef", "name" .= pshow name, "line" .= nodeLine inf]
   toJSON (CTypeOfExpr expr inf) = object ["node" .= pack "CTypeOfExpr", "expr" .= toJSON expr, "line" .= nodeLine inf]
   toJSON (CTypeOfType tipe inf) = object ["node" .= pack "CTypeOfType", "type" .= toJSON tipe, "line" .= nodeLine inf]

instance ToJSON CStructUnion where
   toJSON (CStruct CStructTag ident fields attrs inf) = object $ ["node" .= pack "CStruct", "ident" .= pshow ident, "line" .= nodeLine inf] ++ maybeJSON fields
   toJSON (CStruct CUnionTag ident fields attrs inf) = object $ ["node" .= pack "CUnion", "ident" .= pshow ident, "line" .= nodeLine inf] ++ maybeJSON fields

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
   toJSON (CEnum name enum_list attrs inf) = object ["node" .= pack "CEnum", "name" .= pshow name, "enum-list" .= thelist enum_list, "attrs" .= map toJSON attrs, "line" .= nodeLine inf]
      where
      thelist Nothing = ["Null"]
      thelist (Just elist) = map listToJSON elist
      listToJSON (name, val) = object ["enum" .= pshow name, "val" .= toJSON val]

instance ToJSON CTypeQual where
   toJSON (CConstQual inf) = object ["node" .= pack "CConstQual", "line" .= nodeLine inf]
   toJSON (CVolatQual inf) = object ["node" .= pack "CVolatQual", "line" .= nodeLine inf]
   toJSON (CRestrQual inf) = object ["node" .= pack "CRestrQual", "line" .= nodeLine inf]
   toJSON (CInlineQual inf) = object ["node" .= pack "CInlineQual", "line" .= nodeLine inf]
   toJSON (CAttrQual attr) = object ["node" .= pack "CAttrQual", "attr" .= toJSON attr]

instance ToJSON CAttr where
   toJSON (CAttr name params inf) = object ["node" .= pack "CAttr", "name" .= pshow name, "params" .= map toJSON params, "line" .= nodeLine inf]

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
   toJSON (CComma exprs inf) = object ["node" .= pack "CComma", "exprs" .= map toJSON exprs, "line" .= nodeLine inf]-- comma expr list n >= 2
   toJSON (CAssign op lvalue rvalue inf) = object ["node" .= pack "CAssign", "op" .= toJSON op, "lvalue" .= toJSON lvalue, "rvalue" .= toJSON rvalue, "line" .= nodeLine inf]
   toJSON (CCond cond texpr fexpr inf) = object ["node" .= pack "CCond", "cond" .= toJSON cond, "true" .= toJSON texpr, "false" .= toJSON fexpr, "line" .= nodeLine inf]
   toJSON (CBinary op erand1 erand2 inf) = object ["node" .= pack "CBinary", "op" .= toJSON op, "erand1" .= toJSON erand1, "erand2" .= toJSON erand2, "line" .= nodeLine inf]
   toJSON (CCast decl expr inf) = object ["node" .= pack "CCast", "type" .= toJSON decl, "expr" .= toJSON expr, "line" .= nodeLine inf]
   toJSON (CUnary op expr inf) = object ["node" .= pack "CUnary", "op" .= toJSON op, "expr" .= toJSON expr, "line" .= nodeLine inf]
   toJSON (CSizeofExpr expr inf) = object ["node" .= pack "CSizeofExpr", "expr" .= toJSON expr, "line" .= nodeLine inf]
   toJSON (CSizeofType tipe inf) = object ["node" .= pack "CSizeofType", "type" .= toJSON tipe, "line" .= nodeLine inf]
   toJSON (CAlignofExpr expr inf) = object ["node" .= pack "CAlignofExpr", "expr" .= toJSON expr, "line" .= nodeLine inf]
   toJSON (CAlignofType tipe inf) = object ["node" .= pack "CAlignofType", "type" .= toJSON tipe, "line" .= nodeLine inf]
   toJSON (CComplexReal real inf) = object ["node" .= pack "CComplexReal", "real" .= toJSON real, "line" .= nodeLine inf]
   toJSON (CComplexImag imag inf) = object ["node" .= pack "CComplexImag", "imag" .= toJSON imag, "line" .= nodeLine inf]
   toJSON (CIndex array index inf) = object ["node" .= pack "CIndex", "array" .= toJSON array, "index" .= toJSON index, "line" .= nodeLine inf]
   toJSON (CCall fun args inf) = object ["node" .= pack "CCall", "function" .= toJSON fun, "args" .= map toJSON args, "line" .= nodeLine inf]
   toJSON (CMember struct name deref inf) = object ["node" .= pack "CMember", "name" .= pshow name, "struct" .= toJSON struct, "deref?" .= toJSON deref, "line" .= nodeLine inf]
   toJSON (CVar ident inf) = object ["node" .= pack "CVar", "name" .= pshow ident, "line" .= nodeLine inf]
   toJSON (CConst const) = object ["node" .= pack "CConst", "const" .= toJSON const]
   toJSON (CCompoundLit decl initl inf) = object ["node" .= pack "CCompoundLit", "decl" .= toJSON decl, "init-list" .= map toJSON initl, "line" .= nodeLine inf]
   toJSON (CStatExpr stat _) = toJSON stat
   toJSON (CLabAddrExpr label inf) = object ["node" .= pack "CLabAddrExpr", "label" .= pshow label, "line" .= nodeLine inf]
   toJSON (CBuiltinExpr thing) = object ["node" .= pack "CBuiltinExpr", "expr" .= toJSON thing]

instance ToJSON CBuiltin where
   toJSON (CBuiltinVaArg expr decl inf) = object ["node" .= pack "CBuiltinVaArg", "expr" .= toJSON expr, "type" .= toJSON decl, "line" .= nodeLine inf]
   toJSON (CBuiltinOffsetOf decl desigs inf) = object ["node" .= pack "CBuiltinOffsetOf", "type" .= toJSON decl, "designator-list" .= map toJSON desigs, "line" .= nodeLine inf]
   toJSON (CBuiltinTypesCompatible decl1 decl2 inf) = object ["node" .= pack "CBuiltinTypesCompatible", "type1" .= toJSON decl1, "type2" .= toJSON decl2, "line" .= nodeLine inf]

instance ToJSON CConst where --i
   toJSON (CIntConst int inf) = object ["node" .= pack "CIntConst", "val" .= toJSON int, "line" .= nodeLine inf]
   toJSON (CCharConst char inf) = object ["node" .= pack "CCharConst", "val" .= toJSON char, "line" .= nodeLine inf]
   toJSON (CFloatConst float inf) = object ["node" .= pack "CFloatConst", "val" .= toJSON float, "line" .= nodeLine inf]
   toJSON (CStrConst str inf) = object ["node" .= pack "CStrConst", "val" .= toJSON str, "line" .= nodeLine inf]

instance ToJSON CString where --i
   toJSON (CString str _) = object ["node" .= pack "CString", "string" .= pshow str]

instance ToJSON CStrLit where --i
   toJSON (CStrLit str inf) = object ["node" .= pack "CStrLit", "string" .= toJSON str, "line" .= nodeLine inf]

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
