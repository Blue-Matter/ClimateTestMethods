"""
Builds Data/SA_Anchovy_2024_estimates.xlsx from the PDFs in refs/.

Vector figures (de Moor 2024, FISHERIES/2024/AUG/SWG-PEL/35) are read exactly from the
PDF drawing paths and calibrated against the axis tick marks. Raster figures (de Moor 2025,
IWS/2025/Sardine/BG7) are digitized by pixel tracing. Tables are parsed from the PDF text.

Requirements: python >= 3.9, pymupdf, numpy, scipy, openpyxl, pillow, pdftotext (poppler-utils)
Usage (from docs/OMs/Anchovy):  python -I code/extract_anchovy_figures.py
"""
import io, os, re, subprocess
import numpy as np
import fitz  # pymupdf
import openpyxl
from openpyxl.styles import Font
from PIL import Image
from scipy.optimize import curve_fit, linear_sum_assignment

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
REF = os.path.join(ROOT, "refs")
PDF24 = os.path.join(REF, "FISHERIES_2024_AUG_SWG_PEL_35 AnchovyAssessment.pdf")
PDF25 = os.path.join(REF, "IWS_2025_Sardine_BG7 Assessment of the South African anchovy resource using data up to 2024.pdf")
PDFDATA = os.path.join(REF, "FISHERIES_2024_AUG_SWG-PEL_34 Anchovy Data Record.pdf")
OUT = os.path.join(ROOT, "Data", "SA_Anchovy_2024_estimates.xlsx")
KN = 0.89  # November survey bias, baseline (M=1.2, ABH), de Moor (2024) Table 2
YEARS = list(range(1984, 2024))

# ---------------------------------------------------------------- vector helpers
D24 = fitz.open(PDF24)


def dump(pg, box):
    """Drawing paths and text spans of page pg (0-based) inside box (x0, y0, x1, y1)."""
    p = D24[pg]
    x0, y0, x1, y1 = box
    out = []
    for x in p.get_drawings():
        r = x["rect"]
        if r.x0 >= x0 and r.x1 <= x1 and r.y0 >= y0 and r.y1 <= y1:
            pts = []
            for it in x["items"]:
                if it[0] == "l":
                    pts += [(it[1].x, it[1].y), (it[2].x, it[2].y)]
                elif it[0] == "c":
                    pts += [(it[1].x, it[1].y), (it[4].x, it[4].y)]
            out.append(dict(color=x.get("color"), fill=x.get("fill"), n=len(x["items"]), pts=pts,
                            rect=(r.x0, r.y0, r.x1, r.y1)))
    txt = []
    for b in p.get_text("dict")["blocks"]:
        for l in b.get("lines", []):
            for s in l["spans"]:
                bb = s["bbox"]
                if bb[0] >= x0 - 30 and bb[2] <= x1 + 5 and bb[1] >= y0 - 5 and bb[3] <= y1 + 30 and s["text"].strip():
                    txt.append((s["text"].strip(), bb))
    return out, txt


def num(s):
    try:
        return float(s.replace("‐", "-").replace("−", "-"))
    except ValueError:
        return None


def axes(out, txt):
    """Linear maps from page to data coordinates using tick marks and their numeric labels."""
    xt, yt = [], []
    for o in out:
        if o["n"] == 1 and len(o["pts"]) == 2 and o["color"] == (0, 0, 0):
            (a, b), (c, e) = o["pts"]
            if abs(a - c) < 0.05 and 3 < abs(b - e) < 9:
                xt.append((a, max(b, e)))
            if abs(b - e) < 0.05 and 3 < abs(a - c) < 9:
                yt.append((min(a, c), b))
    lab = [(num(t), bb) for t, bb in txt if num(t) is not None]
    X, Y = [], []
    for x, yb in xt:
        c = [(abs((bb[0] + bb[2]) / 2 - x) + abs(bb[1] - yb), v) for v, bb in lab if yb - 1 < bb[1] < yb + 15]
        if c and min(c)[0] < 22:
            X.append((x, min(c)[1]))
    for xl, y in yt:
        c = [(abs((bb[1] + bb[3]) / 2 - y) + abs(bb[2] - xl), v) for v, bb in lab if xl - 15 < bb[2] < xl + 1]
        if c and min(c)[0] < 22:
            Y.append((y, min(c)[1]))
    X, Y = np.array(sorted(set(X))), np.array(sorted(set(Y)))
    return np.polyfit(X[:, 0], X[:, 1], 1), np.polyfit(Y[:, 0], Y[:, 1], 1)


def tr(fx, fy, pts):
    return [(np.polyval(fx, a), np.polyval(fy, b)) for a, b in pts]


def dedup(pts):
    o = []
    for q in pts:
        if not o or abs(o[-1][0] - q[0]) > 1e-6 or abs(o[-1][1] - q[1]) > 1e-6:
            o.append(q)
    return o


def longest_black_line(out, minn=30):
    return [o for o in out if o["color"] == (0, 0, 0) and o["fill"] is None and o["n"] >= minn][0]


def centre(fx, fy, o):
    return (np.polyval(fx, (o["rect"][0] + o["rect"][2]) / 2), np.polyval(fy, (o["rect"][1] + o["rect"][3]) / 2))


# ---------------------------------------------------------------- de Moor (2024) vector figures
# Figure 1 (page 11): bias-corrected model total biomass at 15 Nov (million t)
out, txt = dump(10, (40, 55, 300, 220)); fx, fy = axes(out, txt)
B_plot = [b for a, b in dedup(tr(fx, fy, longest_black_line(out)["pts"]))]
assert len(B_plot) == 40

# Figure 11 (page 16): recruitment at 1 Oct (billions)
out, txt = dump(15, (170, 530, 420, 745)); fx, fy = axes(out, txt)
R = [b for a, b in dedup(tr(fx, fy, longest_black_line(out)["pts"]))]
assert len(R) == 40

# Figure 10 (page 16): S-R scatter, fitted curve and residual plots
out, txt = dump(15, (55, 40, 305, 265)); fx, fy = axes(out, txt)
SRpts = np.array([centre(fx, fy, o) for o in out if o["n"] == 4 and o["color"] == (0, 0, 0) and o["fill"] is None])
SRcurve = np.array(dedup(tr(fx, fy, [o for o in out if o["n"] > 20][0]["pts"])))
out, txt = dump(15, (45, 280, 300, 450)); fx, fy = axes(out, txt)
resid_year = np.array(sorted(centre(fx, fy, o) for o in out if o["n"] == 3 and o["fill"] == (0, 0, 0)))[:, 1]
out, txt = dump(15, (300, 280, 560, 450)); fx, fy = axes(out, txt)
resid_ssb = np.array([centre(fx, fy, o) for o in out if o["n"] == 3 and o["fill"] == (0, 0, 0)])
# match each year to a (SSB, residual) point: residuals must agree and the S-R scatter point at that SSB must
# have the year's recruitment (2000 is off the top of the S-R panel, R > 1500)
cost = np.zeros((40, 40))
for y in range(40):
    for j in range(40):
        c = ((resid_year[y] - resid_ssb[j, 1]) / 0.03) ** 2
        k = np.where(np.abs(SRpts[:, 0] - resid_ssb[j, 0]) < 0.04)[0]
        c += min(((R[y] - SRpts[k, 1]) / 8) ** 2) if len(k) else (0 if R[y] > 1500 else 1e4)
        cost[y, j] = c
_, col = linear_sum_assignment(cost)
SSB = resid_ssb[col, 0]

# Figure 6 (page 13): quarterly commercial selectivity-at-length, ABH panel
out, txt = dump(12, (100, 60, 300, 215)); fx, fy = axes(out, txt)
sel = {}
for q, colr in [("Q1", (0, 0, 0)), ("Q2", (1.0, 0.0, 0.0)), ("Q3", (0.0, 1.0, 0.0)), ("Q4", (0.0, 0.0, 1.0))]:
    pts = []
    for o in out:
        if o["color"] == colr and o["fill"] is None and not (colr == (0, 0, 0) and o["n"] < 5):
            pts += o["pts"]
    sel[q] = np.array(sorted(set((round(a, 3), round(b, 4)) for a, b in tr(fx, fy, pts))))

# Figure 9 (page 15): proportion-at-length of ages 0, 1, 2 by quarter
MIDS = np.arange(1.75, 16.26, 0.5)
qcol = {(0.0, 0.0, 0.0): "Q1", (1.0, 0.0, 0.0): "Q2", (0.0, 1.0, 0.0): "Q3", (0.0, 0.0, 1.0): "Q4"}
ALK = {}
for nm, box in {"A0": (305, 295, 545, 440), "A1": (55, 468, 295, 614), "A2": (305, 468, 545, 614)}.items():
    out, txt = dump(14, box); fx, fy = axes(out, txt)
    by = {}
    for o in out:
        if o["fill"] is not None or o["color"] is None or not o["pts"]:
            continue
        if o["color"] == (0, 0, 0) and o["n"] == 1:
            (a, b), (c, e) = o["pts"]
            if (abs(a - c) < 0.05 and abs(b - e) > 3) or (abs(b - e) < 0.05 and abs(a - c) > 3.5):
                continue  # axes and ticks
        if o["rect"][1] < box[1] + 30 and o["rect"][0] > box[0] + 150:
            continue  # legend
        by.setdefault(qcol[o["color"]], []).extend(o["pts"])
    for q, p in by.items():
        P = np.array(sorted(set((round(a, 4), round(b, 5)) for a, b in tr(fx, fy, p))))
        v = np.clip(np.interp(MIDS, P[:, 0], P[:, 1]), 0, None)
        ALK[f"{nm}_{q}"] = v / v.sum()
ALK["A0_Q4"] = np.r_[1.0, np.zeros(len(MIDS) - 1)]  # just-spawned recruits: minus group

# ---------------------------------------------------------------- de Moor (2025) raster figures
D25 = fitz.open(PDF25)


def page_image(pg, k):
    x = D25[pg].get_images()[k]
    pix = fitz.Pixmap(D25, x[0])
    if pix.n > 4:
        pix = fitz.Pixmap(fitz.csRGB, pix)
    return np.array(Image.open(io.BytesIO(pix.tobytes("png"))).convert("L"))


# Figure 12 (page 15, image 2): harvest proportion 1984-2024
a = page_image(14, 1) < 130
X = lambda yr: 100 + (yr - 1990) * (281 - 100) / 30          # x ticks: 1990 at px 100, 2020 at px 281
Y = lambda row: (226 - row) / ((226 - 23) / 0.6)             # y ticks: 0 at px 226, 0.6 at px 23
yrs25 = list(range(1984, 2025))
runs = []
for yr in yrs25:
    rows = np.where(a[8:226, int(round(X(yr)))])[0] + 8
    runs.append((rows.min(), rows.max(), rows.mean()))
runs = np.array(runs)
U25 = []
for i in range(len(yrs25)):
    nb = [runs[j, 2] for j in (i - 1, i + 1) if 0 <= j < len(yrs25)]
    m = runs[i, 2]
    r = runs[i, 0] + 1 if all(m < v for v in nb) else runs[i, 1] - 1 if all(m > v for v in nb) else m
    U25.append(round(Y(r), 4))

# Figure 8 (page 12, image 1): growth curve, fitted with a von Bertalanffy model
a = page_image(11, 0) < 100
pts = []
for j in range(78, 240):
    rows = [i for i in range(16, 140) if a[i, j]]
    if rows:
        pts.append(((j - 76) / (241 - 76) * 5, (141 - np.mean(rows)) / (141 - 15) * 12))
pts = np.array(pts)
keep = (pts[:, 0] < 1.8) & (pts[:, 1] > 0.3)
vb = lambda t, L, k, t0: L * (1 - np.exp(-k * (t - t0)))
VB, _ = curve_fit(vb, pts[keep, 0], pts[keep, 1], p0=[12, 2, 0.2])

# ---------------------------------------------------------------- tables
txt25 = subprocess.run(["pdftotext", "-layout", PDF25, "-"], capture_output=True, text=True).stdout
seg = txt25[txt25.index("Table B.3. Annual length"):][:4000]
MAT = sorted(set((int(m.group(1)), float(m.group(2)), float(m.group(3)))
                 for l in seg.splitlines()
                 for m in re.finditer(r"\b(19[89]\d|20[0-2]\d)\s*(?:2[1-7])?\s+(\d+\.\d{3})\s+(\d\.\d{3})", l)))
assert len(MAT) == 41

txtD = subprocess.run(["pdftotext", "-layout", PDFDATA, "-"], capture_output=True, text=True).stdout
MON = {}
for l in txtD.splitlines():
    m = re.match(r"\s+(19[89]\d|20[0-2]\d)\s+((?:[\d.]+\s+){11}[\d.]+)\s*$", l)
    if m:
        MON[int(m.group(1))] = [float(v) for v in m.group(2).split()]
assert len(MON) == 40
i, j = txtD.index("Table 2. Anchovy total biomass"), txtD.index("Table 3. The date")
SURV = {}
for l in txtD[i:j].splitlines():
    m = re.match(r"\s+(19[89]\d|20[0-2]\d|Feb 2024)\s+([\d ]+?)\s+(0\.\d{3})(?:\s+(\d+)\s+(0\.\d{3}))?\s*$", l)
    if m:
        SURV[2023 if m.group(1) == "Feb 2024" else int(m.group(1))] = (
            int(m.group(2).replace(" ", "")), float(m.group(3)),
            int(m.group(4)) if m.group(4) else None, float(m.group(5)) if m.group(5) else None)
REC = {}
for l in txtD[j:j + 12000].splitlines():
    m = re.match(r"\s+(19[89]\d|20[0-2]\d)\s+.*?\s(\d\.\d{3})\s+<\s*([\d.]+)\s+([\d ]+?)\s+(0\.\d{3})\s+([\d.]+)\s+([\d.]+)\s*$", l)
    if m:
        REC[int(m.group(1))] = (float(m.group(7)), float(m.group(5)))  # numbers (billions), CV
assert len(REC) == 38  # 1985-2023, no survey in 2018

# ---------------------------------------------------------------- write workbook
wb = openpyxl.Workbook()
bold = Font(bold=True)


def sheet(name, head, rows):
    ws = wb.create_sheet(name)
    ws.append(head)
    for c in ws[1]:
        c.font = bold
    for r in rows:
        ws.append(r)


rows = []
for k, y in enumerate(YEARS):
    os_ = sum(MON[y][9:]) + sum(MON[y + 1][:9]) if y + 1 in MON else None
    s, r = SURV.get(y), REC.get(y)
    rows.append([y, round(B_plot[k], 4), round(B_plot[k] / KN, 4), round(float(SSB[k]), 4), round(R[k], 2),
                 round(sum(MON[y]), 3), None if os_ is None else round(os_, 3),
                 s[0] / 1e6 if s else None, s[1] if s else None,
                 s[2] / 1e6 if s and s[2] else None, s[3] if s and s[3] else None,
                 r[0] if r else None, r[1] if r else None])
sheet("ByYear", ["Year", "B_survey_scale", "B", "SSB", "R", "Catch_cal", "Catch_OctSep", "SurveyB", "SurveyB_CV",
                 "DEPM_SSB", "DEPM_CV", "RecruitN", "Recruit_CV"], rows)
sel_m = {q: np.interp(MIDS, sel[q][:, 0], np.clip(sel[q][:, 1], 0, 1)) for q in sel}
alk_cols = [f"A{a}_{q}" for a in range(3) for q in ["Q1", "Q2", "Q3", "Q4"]]
sheet("ByLength", ["Length", "Sel_Q1", "Sel_Q2", "Sel_Q3", "Sel_Q4"] + alk_cols,
      [[float(MIDS[k])] + [round(float(sel_m[q][k]), 4) for q in ["Q1", "Q2", "Q3", "Q4"]] +
       [round(float(ALK[c][k]), 5) for c in alk_cols] for k in range(len(MIDS))])
sheet("Maturity", ["Year", "L50", "delta"], [list(m) for m in MAT])
sheet("QuarterCatch", ["Year", "Q4_OctDec", "Q1_JanMar", "Q2_AprJun", "Q3_JulSep"],
      [[y, round(sum(MON[y][9:12]), 3), round(sum(MON[y + 1][0:3]), 3), round(sum(MON[y + 1][3:6]), 3),
        round(sum(MON[y + 1][6:9]), 3)] for y in range(1984, 2023)])
sheet("Assess2025", ["Year", "U"], [[y, u] for y, u in zip(yrs25, U25)])
sheet("SRcurve", ["SSB", "R"], [[round(float(a), 4), round(float(b), 2)] for a, b in SRcurve])
sheet("Parameters", ["Name", "Value", "Source"], [
    ["M", 1.2, "Natural mortality, ages 0 and 1+ (year-1); de Moor (2025) Table B.1 / de Moor (2024)"],
    ["kN", KN, "November acoustic survey multiplicative bias at the joint posterior mode, baseline M=1.2 ABH; de Moor (2024) Table 2"],
    ["w_a", 0.007366, "Mass-at-length (g = w_a * cm^w_b), November; de Moor (2023), de Moor (2025) Table B.1"],
    ["w_b", 3.11124, "as above"],
    ["Linf", round(float(VB[0]), 2), "von Bertalanffy fit to digitized de Moor (2025) Fig. 8 (cm)"],
    ["K", round(float(VB[1]), 3), "as above (year-1)"],
    ["t0", round(float(VB[2]), 3), "as above (year); de Moor (2025) reports t0 = 0.19"],
    ["sigmaR_2025_A2BH_early_late", 0.74, "de Moor (2025b, P2) Fig. 1b"],
    ["sigmaR_2025_A2BH_middle", 0.43, "de Moor (2025b, P2) Fig. 1b"]])
sheet("Sources", ["Sheet", "Column", "Description"], [
    ["ByYear", "Year", "Assessment year y: spawning/recruitment at 1 Oct y, November survey y. In the OM, year y runs Oct y to Sep y+1."],
    ["ByYear", "B_survey_scale", "Bias-corrected (k_N * B) model-estimated total biomass at 15 Nov (million t); ABH, de Moor (2024) Fig. 1, extracted from vector graphics. 2023 corresponds to Feb 2024."],
    ["ByYear", "B", "B_survey_scale / kN: model total biomass at 15 Nov (million t)"],
    ["ByYear", "SSB", "Spawner biomass at 1 Oct (million t); de Moor (2024) Fig. 10, matched to years via the residual plots"],
    ["ByYear", "R", "Recruitment at 1 Oct (billions); de Moor (2024) Fig. 11 (vector). Final year (2023) poorly estimated."],
    ["ByYear", "Catch_cal", "Calendar-year anchovy landings (thousand t); de Moor et al. (2024) Table 1"],
    ["ByYear", "Catch_OctSep", "Landings Oct y to Sep y+1 (thousand t), i.e. OM year y; NA for 2023 (2024 monthly catch not tabulated)"],
    ["ByYear", "SurveyB, SurveyB_CV", "November hydroacoustic total biomass (million t) and CV; de Moor et al. (2024) Table 2. 2023 = Feb 2024 survey."],
    ["ByYear", "DEPM_SSB, DEPM_CV", "Daily egg production method spawner biomass (million t) and CV, 1984-1993; de Moor et al. (2024) Table 2"],
    ["ByYear", "RecruitN, Recruit_CV", "May/June recruit survey numbers (billions) and CV; de Moor et al. (2024) Table 3 (no survey in 2018)"],
    ["ByLength", "Length", "Length class mid-point (cm), 0.5 cm classes; first class is the 1.5- cm minus group"],
    ["ByLength", "Sel_Q1..Sel_Q4", "Quarterly commercial selectivity-at-length, ABH; de Moor (2024) Fig. 6 (vector)"],
    ["ByLength", "A0_Q1..A2_Q4", "Proportion-at-length of ages 0,1,2 at the middle of each quarter (Q1 Jan-Mar ... Q4 Oct-Dec); de Moor (2024) Fig. 9 (vector). Age 0 in Q4 is placed in the minus group."],
    ["Maturity", "L50, delta", "Annual logistic maturity-at-length parameters; de Moor (2025) Table B.3 (Yemane et al. 2024)"],
    ["QuarterCatch", "Q4..Q3", "Landings (thousand t) by quarter for OM year y (Q4 = Oct-Dec y; Q1-Q3 = Jan-Sep y+1); de Moor et al. (2024) Table 1"],
    ["Assess2025", "U", "Historical harvest proportion, ABH, data to 2024; de Moor (2025) Fig. 12, digitized from a raster image (approx. +/-0.005)"],
    ["SRcurve", "SSB, R", "Points along the fitted ABH Beverton-Holt curve (million t, billions); de Moor (2024) Fig. 10 (vector)"]])
del wb["Sheet"]
wb.save(OUT)
print("Wrote", OUT)
print("Mean recruitment 1984-2022 (caption: 610 billion):", round(float(np.mean(R[:-1])), 1))
