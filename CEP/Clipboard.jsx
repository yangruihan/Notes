var docRef = app.activeDocument;
docRef.artLayers[0].copy();

var newDocRef = app.documents.add(8, 6, 72, "new doc");
newDocRef.paste();

