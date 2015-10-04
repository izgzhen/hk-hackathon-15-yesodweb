module Handler.Image where

import Aliyun.Config
import Aliyun.OSS as OSS
import Import
import Yesod.Form.Bootstrap3

writeToServer :: FileInfo -> IO FilePath
writeToServer file = do
    let filename = unpack $ fileName file
        path = "static/tmp" </> filename

    fileMove file path
    t <- readFile path :: IO ByteString
    Just config <- readConfig "/root/.aliyuncli"
    void $ putObject t config "codehkupload" filename CnHongKong
    return filename


removeFromServer :: FilePath -> IO ()
removeFromServer fp = do
    Just config <- readConfig "/root/.aliyuncli"
    void $ deleteObject config "codehkupload" fp CnHongKong

checkFileExist :: FilePath -> IO Bool
checkFileExist _ = return True

getFiles :: IO [FilePath]
getFiles = do
    Just config <- readConfig "/root/.aliyuncli"
    listObjects config "codehkupload" CnHongKong

getPath :: FilePath -> FilePath
getPath fp = fp

------------------------------------------------------------



uploadForm :: Form (FileInfo, Maybe Textarea)
uploadForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> fileAFormReq "Image file"
    <*> aopt textareaField "Image description" Nothing

addStyle :: Widget
addStyle = do
    -- Twitter Bootstrap
    addStylesheetRemote "http://netdna.bootstrapcdn.com/twitter-bootstrap/2.1.0/css/bootstrap-combined.min.css"
    -- message style
    toWidget [lucius|.message { padding: 10px 0; background: #ffffed; } |]
    -- jQuery
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.8.0/jquery.min.js"
    -- delete function
    toWidget [julius|
            $(function(){
                function confirmDelete(link) {
                    if (confirm("Are you sure you want to delete this image?")) {
                        deleteImage(link);
                    };
                }
                function deleteImage(link) {
                    $.ajax({
                        type: "DELETE",
                        url: link.attr("data-img-url"),
                    }).done(function(msg) {
                        var table = link.closest("table");
                        link.closest("tr").remove();
                        var rowCount = $("td", table).length;
                        if (rowCount === 0) {
                            table.remove();
                        }
                    });
                }
                $("a.delete").click(function() {
                    confirmDelete($(this));
                    return false;
                });
            });
        |]

getImagesR :: Handler Html
getImagesR = do
    ((_, widget), enctype) <- runFormPost uploadForm
    images <- liftIO getFiles
    mmsg <- getMessage
    defaultLayout $ do
        addStyle
        [whamlet|$newline never
                $maybe msg <- mmsg
                    <div .message>
                        <div .container>
                            #{msg}
                <div .container>
                    <div .row>
                        <h2>
                            Upload new image
                        <div .form-actions>
                            <form method=post enctype=#{enctype}>
                                ^{widget}
                                <input .btn type=submit value="Upload">
                        $if not $ null images
                            <table .table>
                                <tr>
                                    <th>
                                        Image
                                    <th>
                                        Delete Action
                                $forall image <- images
                                    <tr>
                                        <td>
                                            <a href=#{getPath image}>
                                                #{show image}
                                        <td>
                                            <a href=# .delete data-img-url=@{ImageR image}>
                                                delete

        |]

postImagesR :: Handler Html
postImagesR = do
    ((result, _), _) <- runFormPost uploadForm
    case result of
        FormSuccess (file, _) -> do
            -- TODO: check if image already exists
            -- save to image directory
            void $ liftIO $ writeToServer file
            setMessage "Image saved"
            redirect ImagesR
        _ -> do
            setMessage "Something went wrong"
            redirect ImagesR

deleteImageR :: String -> Handler ()
deleteImageR imageName = do
    -- only delete from database if file has been removed from server
    -- stillExists <- liftIO $ checkFileExist (unpack imageName)

    -- case (not stillExists) of
        -- False -> redirect ImagesR
        -- True  -> do
    liftIO $ removeFromServer (unpack imageName)
    setMessage "Image has been deleted."
    redirect ImagesR
