"""Versioned project labels and deliberately approximate camera scale context."""
import re

RENAMES={'thin fyi':'thin ice floe','ice floe':'thick ice floe',
         'smooth water':'calm water','smooth open water':'calm open water'}
PATTERN=re.compile(r'\b(?:'+ '|'.join(map(re.escape,RENAMES)) +r')\b')

def rename(text):
    # One pass avoids turning the newly inserted "thin ice floe" into "thin thick ice floe".
    return PATTERN.sub(lambda match:RENAMES[match.group()],text)

SCALE = {
    'status':'rough assumed scale, not calibrated',
    'camera_height_m':[9,12],
    'height_basis':'Eric estimates three stories; interpreted provisionally as 9–12 m above waterline',
    'camera_model':'AXIS P3807-PVE (JPEG EXIF)',
    'nominal_fov_deg':[180,90],
    'assumed_downward_tilt_deg':25,
    'tilt_sensitivity_deg':[20,30],
    'approximate_roi_extent_m':[50,25],
    'uncertainty':'Order of magnitude only; allow roughly a factor of two, especially at the far edge',
    'assumptions':'Flat sea; nominal panoramic angular mapping; approximate tilt inferred from horizon; unknown stitching/cropping calibration, ship roll/pitch, and exact mounting height',
    'source':'https://www.axis.com/en-us/products/axis-p3807-pve/support',
}

KEY = '''Image 2 follows: a reference key, not the target. Its rows contain three
human-selected appearance examples per label. The labels have been renamed;
these example crops are appearance references, NOT independently measured size
or thickness standards. Use these project-specific categories:
- grease ice: a smooth or finely granular/slushy-looking surface, sometimes with
  slick streaks and subdued ripples; can be visually inseparable from calm water.
- nilas: thin-looking dark/gray continuous film or sheets, sometimes with visible
  cracks, overlapping edges or flexible-looking broken plates.
- thin ice floe: a coherent piece estimated MORE THAN 20 m across, with relatively
  thin-looking, flat sheet appearance matching the key's thin ice floe row.
- thick ice floe: a coherent piece estimated MORE THAN 20 m across, with relatively
  substantial/opaque or snow-covered appearance matching the thick ice floe row.
  Thin/thick are visual appearance labels, not measured thickness or confirmed age.
- icy bits: sparse, isolated small fragments with water visible between them.
- brash ice: concentrations of broken/jumbled pieces individually estimated LESS
  THAN 20 m across. Assess the pieces, not the overall width of a rubble field.
The 20 m split is this project's convention, not a claim of standard ice nomenclature.
Near 20 m, or when size/thickness cannot be distinguished, acknowledge uncertainty
and use unknown where necessary rather than inventing precise measurements.
Do not classify the key, green text, borders, or title. Each example includes its
own background; a category label does not imply 100% coverage. Do not force grease
ice when calm water is ambiguous. Grease ice and nilas remain surface-film classes;
the piece-size rule distinguishes brash from the two floe classes.'''

ROI_SCALE_PROMPT = '''
ROUGH SCALE FOR IMAGE 3 (a provisional aid, not a measurement):
The camera is provisionally about 9–12 m above the waterline (roughly three stories).
The original 1200×600-pixel rotated footprint is estimated to span on the order of
50 m along its longer ground extent and 25 m across. This is a tapered,
perspective-distorted footprint, NOT a uniform 50×25 m rectangle. Allow roughly
a factor-of-two uncertainty, especially near the far edge; the mounting height,
panoramic projection and camera attitude are not calibrated. The 2400×1200 input
is an enlargement of that SAME footprint and does not double the physical size.
Use image 1 to follow a piece beyond the ROI when estimating whether the whole
piece is above or below 20 m across; still count ONLY its area inside image 3 in
the percentages. Do not classify a large floe as small brash merely because only
a small part is visible inside the ROI. Do not assign a constant metres-per-pixel
scale or pretend to have measured thickness. Percentages remain IMAGE-PLANE
coverage, not perspective-corrected ground-area concentration.
'''

def build_prompt(texts):
    output=[rename(text) for text in texts]
    output[1]=KEY
    output[2]+=ROI_SCALE_PROMPT
    return output
