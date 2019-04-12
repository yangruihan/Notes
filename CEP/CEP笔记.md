# CEP 笔记

CEP (Common Extensibility Platform)

## Extension Type

- Panel

- ModalDialog

- Modeless

- Custom

## Browser Features supported by CEP

Http Cookie

- Session Cookies - Temporary in-memory cookie which will expire and disappear when user closes extension

- Persistent Cookies - No expiry date or validity interval, stored in user's file system

## Startup Scripts

On startup, Photoshop executes all .jsx files that it finds in the startup folders.

- Windows: `C:\Program Files\Common Files\Adobe\Startup Scripts CC\Adobe Photoshop`

- MacOS: `C:\Program Files\Common Files\Adobe\Startup Scripts CC\Adobe Photoshop`

## Containment hierarchy

```
Application
     |
     - Documents collection
               |
               - Document 
               |
               - Document
               |      |
               ...    - ArtLayers collection
                      |
                      - HistoryStates collection
                      |
                      - Layers collection
                      |
                      - Layersets collection
                      |
                      - Channels collection
```

![](images/p1.png)

- `Art Layer` 对应的是有图片的层

- `Layer set` 对应的是Layer Group

## Script Tips

- creating new objects in script: `add()`

  ```[js]
  // create document
  app.documents.add();

  // create artLayer
  document.artLayers.add();
  ```

- setting the active object

  ```[js]
  // Create 2 documents
  var docRef = app.documents.add(4, 4);
  var otherDocRef = app.documents.add (4, 6);

  //make docRef the active document
  app.activeDocument = docRef;
  //here you would include command statements
  //that perform actions on the active document. Then, you could
  //make a different document the active document
  docRef.activeLayer = docRef.layers[0];

  //use the activeDocument property of the Application object to
  //bring otherDocRef front-most as the new active document
  app.activeDocument = otherDocRef;
  ```

- open document: `open()`

  ```
  var fileRef = File(app.path + "/Samples/test.psd");
  var docRef = app.open(fileRef);
  ```

  打开文件选项:

  ![](/images/p2.png)

  ```[js]
  // Set the ruler units to pixels
  var originalRulerUnits = app.preferences.rulerUnits;
  app.preferences.rulerUnits = Units.PIXELS;

  // Get a reference to the file that we want to open
  var fileRef = new File(“/c/pdffiles/myfile.pdf”);

  // Create a PDF option object
  var pdfOpenOptions = new PDFOpenOptions;
  pdfOpenOptions.antiAlias = true;
  pdfOpenOptions.mode = OpenDocumentMode.RGB;
  pdfOpenOptions.resolution = 72;
  pdfOpenOptions.page = 3;
  // open the file
  app.open( fileRef, pdfOpenOptions );

  // restore unit settings
  app.preferences.rulerUnits = originalRulerUnits
  ```

- save document:

  ```[js]
  app.documents.add( 4, 4 );
  jpgFile = new File( "/Temp001.jpeg" );
  jpgSaveOptions = new JPEGSaveOptions();
  jpgSaveOptions.embedColorProfile = true;
  jpgSaveOptions.formatOptions = FormatOptions.STANDARDBASELINE;
  jpgSaveOptions.matte = MatteType.NONE;
  jpgSaveOptions.quality = 1;
  app.activeDocument.saveAs(jpgFile, jpgSaveOptions, true, Extension.LOWERCASE);
  ```

  保存文件选项：

  ![](./images/p3.png)

