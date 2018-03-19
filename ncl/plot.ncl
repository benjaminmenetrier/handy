load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

; Parameters:
; ----------

society = "egalitarian"
config = "oscillatory_landing"
scheme = "rk"
nt = 1000
dtrat = 1
lpos = 3
precision = "sp"

; Load data:
; ----------
 
filename = society + "-" + config + "-" + scheme + "-" + sprinti("%0.2i",dtrat) + "-lpos" + lpos + "-" + precision
data = asciiread("../data/" + filename + ".dat",(/nt*dtrat+1,6/),"float")
data@_FillValue = -999.0
time = data(:,1)
to_plot = transpose(data(:,(/2,4,5/)))
to_plot(2,:) = to_plot(2,:)/10.0

; Resources :
; -----------

xyres = True
xyres@gsnFrame = False  
xyres@gsnDraw = False  
xyres@tmXBLabelFontHeightF = 0.02
xyres@tmYLLabelFontHeightF = 0.02
xyres@xyLineThicknessF = 3.0
xyres@xyCurveDrawOrder = "PreDraw"
xyres@tiXAxisString = "Time"
xyres@trXMinF = min(time)
xyres@trXMaxF = max(time)
xyres@trYMinF = -0.1
xyres@trYMaxF = 2.0
xyres@gsnYRefLine = 0.0
xyres@xyMonoDashPattern = True
xyres@xyDashPattern = 0
xyres@xyLineColors = (/"blue","green","gray"/)
xyres@vpHeightF = 0.4
xyres@vpWidthF = 0.8

; Display :
; ---------

output = filename
wks = gsn_open_wks("epsi",output)
gsn_define_colormap(wks,"BlAqGrYeOrReVi200")

plot = gsn_csm_xy(wks,time,to_plot,xyres)

draw(plot)
frame(wks)
delete(wks)

system("epstopdf " + output + ".epsi;rm -f " + output + ".epsi")

end
