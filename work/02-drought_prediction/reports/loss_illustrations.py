import matplotlib.pyplot as plt
import numpy as np

# consistent colors with the slide
cMSE = (60 / 255, 60 / 255, 60 / 255)
cPIN = (198 / 255, 64 / 255, 42 / 255)
cHIN = (40 / 255, 110 / 255, 160 / 255)

TAU = 0.20
LAM = 5.0


def style(ax, xlabel=r"$\hat{y}_i-y_i$"):
    ax.axhline(0, color="gray", lw=0.8)
    ax.axvline(0, color="gray", lw=0.8)
    ax.set_xlabel(xlabel, fontsize=9, style="italic", loc="right")
    ax.spines[["top", "right"]].set_visible(False)
    ax.tick_params(labelsize=8)


# ---- MSE: L(e) = e^2 ----
fig, ax = plt.subplots(figsize=(2.25, 2.0))
e = np.linspace(-1.55, 1.55, 200)
ax.plot(e, e**2, color=cMSE, lw=2)
ax.set_ylim(0, 2.4)
ax.set_xlim(-1.6, 1.6)
ax.set_ylabel(r"$\mathcal{L}_{MSE}$", fontsize=9)
style(ax)
fig.tight_layout()
fig.savefig("reports/mse.pdf", bbox_inches="tight")

# ---- Pinball tau=0.20 ----
# u = y - yhat = -e ; rho = max(tau*u, (tau-1)*u)
# in terms of e: e>=0 (over-predict, too wet) -> slope (1-tau)=0.8
#                e<0  (under-predict, too dry) -> slope tau=0.2
fig, ax = plt.subplots(figsize=(2.25, 2.0))
e = np.linspace(-1.55, 1.55, 400)
u = -e
rho = np.maximum(TAU * u, (TAU - 1) * u)
ax.plot(e, rho, color=cPIN, lw=2)
ax.set_ylim(0, 1.4)
ax.set_xlim(-1.6, 1.6)
ax.set_ylabel(r"$\mathcal{L}_{pin}$", fontsize=9)
ax.text(-0.95, 0.3, r"slope $\tau$", fontsize=7, color=cPIN)
ax.text(0.55, 0.3, r"slope $1-\tau$", fontsize=7, color=cPIN)
style(ax)
fig.tight_layout()
fig.savefig("reports/pinball.pdf", bbox_inches="tight")

# ---- Hinge-weighted MSE: L(e; y) = (1 + lam*max(0,-y)) * e^2 ----
# weight depends on the TARGET, not the error -> family of parabolas

cHIN = (40 / 255, 110 / 255, 160 / 255)

# ---------- (a) weight profile w(y) ----------
y = np.linspace(-2.5, 1.5, 400)


def weight(y, lam):
    return 1 + lam * np.clip(-y, 0, None)


fig, ax = plt.subplots(figsize=(2.0, 2.0))
ax.plot(y, weight(y, 1), color=cHIN, alpha=0.5, lw=2, label=r"$\lambda=1$")
ax.plot(y, weight(y, 5), color=cHIN, lw=2, label=r"$\lambda=5$")
ax.axhline(0, color="gray", lw=0.8)
ax.axvline(0, color="gray", lw=0.8)
ax.set_xlabel(r"true SPEI $y_i$", fontsize=9, loc="right")
ax.set_ylabel(r"weight $w_i$", fontsize=9)
ax.set_xlim(-2.5, 1.5)
ax.set_ylim(0, 12)
ax.legend(fontsize=7, frameon=False, loc="upper right")
ax.spines[["top", "right"]].set_visible(False)
ax.tick_params(labelsize=8)
fig.tight_layout()
fig.savefig("reports/hinge_weight.pdf", bbox_inches="tight")

# ---------- (b) loss vs error at three fixed weights ----------
e = np.linspace(-1.55, 1.55, 300)
fig, ax = plt.subplots(figsize=(2.25, 2.0))
for w, shade, lbl in [(1, 1, r"$w{=}1$ (MSE)"), (2, 0.70, r"$w{=}2$"), (4, 0.40, r"$w{=}4$")]:
    ax.plot(e, w * e**2, color=cHIN, alpha=shade, lw=2, label=lbl)
ax.axhline(0, color="gray", lw=0.8)
ax.axvline(0, color="gray", lw=0.8)
ax.set_xlabel(r"$\hat{y}_i-y_i$", fontsize=9, style="italic", loc="right")
ax.set_ylabel(r"$\mathcal{L}_{hwMSE}$", fontsize=9)
ax.set_xlim(-1.6, 1.6)
ax.set_ylim(0, 2.4)
ax.legend(fontsize=7, frameon=False, loc="upper center")
ax.spines[["top", "right"]].set_visible(False)
ax.tick_params(labelsize=8)
fig.tight_layout()
fig.savefig("reports/hinge_loss.pdf", bbox_inches="tight")
