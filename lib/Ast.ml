

type dummy = A | B | C | D

type nodeMemberList = dummy
type nodeIdentifier = string
type nodeImportAlias = dummy

type nodeUseStatement = {alias: nodeImportAlias option; ident:nodeIdentifier; members:nodeMemberList option; qualify:bool}
type nodeImports = {import: nodeUseStatement; imports: nodeImports option}
type nodeSubUnit = {imports: nodeImports option; declarations: dummy option}

type astNode = UseStatement of nodeUseStatement
             | Imports of nodeImports
             | SubUnit of nodeSubUnit 
             

