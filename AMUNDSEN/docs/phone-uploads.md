# Phone photo uploads

In Photos → Submit → Import from: This device, browse to the destination folder on the
ship's share, select photos, optionally name the batch, and choose **Upload
Selected Photos**. Each batch creates a fresh subfolder with the exact supplied
name. An existing name is refused; choose another name rather than overwriting.
Files get numeric prefixes so duplicate camera filenames
remain distinct. Original file bytes, including embedded metadata, are retained.

This uses the local dashboard connection, including the IP-address `/underway/`
route; it does not require a cloud service or phone file-manager app. Keep the
page open and the phone awake. Retry continues the current batch, and repeating
a successfully saved file is safe. Starting a new selection does not remove
previously uploaded files. Refreshing/closing the page loses the in-memory phone
selection; it does not delete files already saved.

Supported: JPEG, PNG and WebP, up to 300 photos, 64 MiB per photo, 80 megapixels
per image and 2 GiB per batch. HEIC and videos are not supported in this version.

After upload, the browser opens the new folder. Journal publication is a
separate action using the existing photographer/organisation/licence/clock form.
The hidden `.phone-upload` marker prevents a parent-folder import or watch from
inheriting these photos under somebody else's credit/licence. Direct import or
watch of the batch folder still works. Leave the marker in place.

The dashboard service account needs write access to the selected destination.
Session manifests are stored outside the web root in `DB_DIR/photos/uploads.sqlite`
and expire after 24 hours; expiration never deletes share photos. Files are
staged on local temporary disk, checked with Pillow, then copied via hidden
temporary files on the share. Upload endpoints reject cross-site browser requests
and require a per-batch random capability for file writes. As with the existing
ship dashboard, this feature is intended for the trusted ship LAN, not public
internet exposure or untrusted guest networks.

For existing photos, choose **Import from: /Share folder**. The optional
**Keep importing from this /Share folder** switch applies the selected credit
and licence to future arrivals.

Your current import shows a progress bar and final added/skipped/failed counts;
the result persists across page refreshes. **Imports Status** is collapsed by
default. Decode failures are recorded with SHA-256 hashes in
`DB_DIR/photos/failed-images.json`. Manual imports and watched folders skip those
files while their contents are unchanged; changed files are eligible again.
Other images continue importing. Service-wide failures do not blacklist photos.
Older failures without a saved hash may be attempted once to establish it.
