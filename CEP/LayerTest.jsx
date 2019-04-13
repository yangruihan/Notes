var docRef = app.documents.add();
docRef.artLayers.add();

var layerSetRef = docRef.layerSets.add();
var layerRef = docRef.artLayers[0].duplicate(layerSetRef, ElementPlacement.PLACEATEND);

var textLayerRef = docRef.artLayers.add();
textLayerRef.name = "my text layer";
textLayerRef.kind = LayerKind.TEXT;

var textItemRef = textLayerRef.textItem;
textItemRef.contents = "Hello, Text Layer!";
textItemRef.justification = Justification.LEFT;

