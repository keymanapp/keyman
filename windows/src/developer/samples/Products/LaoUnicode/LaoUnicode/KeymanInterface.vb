Module KeymanInterface

    Const ProductID = 1096

    Class KeymanInterface
        Public kmcom As kmcomapi.ITavultesoftKeyman
        Public ActiveProduct As kmcomapi.IKeymanProduct

        Public Sub New()
            kmcom = CType(CreateObject("kmcomapi.TavultesoftKeyman"), kmcomapi.ITavultesoftKeyman)
            ActiveProduct = kmcom.Products.ItemsByProductID(ProductID)
        End Sub
    End Class

End Module
