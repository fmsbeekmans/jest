#!/usr/bin/python
import Image, ImageDraw, ImageFont, os, sys, math, shutil

glyphs = [
    ("g","grass"), #grass
    ("d","dirt"), #dirt
    ("s","snow"), #snow

    # buildings
    ("S-tru","spawn-truck"),
    ("S-tra","spawn-train"),
    ("S-b","spawn-boat"),
    ("Su","supply"),
    ("D","depot"),

    # tiled only
    ("ro<","road-west"),
    ("ro>","road-east"),
    ("rov","road-south"),
    ("ro^","road-north"),
    ("c<","canal-west"),
    ("c>","canal-east"),
    ("cv","canal-south"),
    ("c^","canal-north"),
    ("ra<","rails-west"),
    ("ra>","rails-east"),
    ("rav","rails-south"),
    ("ra^","rails-north"),
    # vehicles
    ("Tu>","truck"),
    ("Ta>","train"),
    ("B>","boat"),

    # rest
    ("", "default"),
    ("M","mixer")
    ]

sane_fill = 2
box=(48,48)

def i_name(i, g):
    fmt = ("%0" + str(sane_fill) +"d")
    return "part-" + fmt % i + g + ".png"

def create_letter(p,g, f):
    fname=i_name(p,g)
    override_fname="override/" + fname
    if os.path.isfile(override_fname):
        shutil.copyfile(override_fname,fname)
    else:
        i = Image.new("RGB", box, "magenta")
        d = ImageDraw.Draw(i)
        mask=Image.new('L', i.size, color=255)
        alpha_draw=ImageDraw.Draw(mask)
        alpha_draw.rectangle(i.getbbox(), fill=0)
        i.putalpha(mask)
        d.text((0,0),g, font=f, fill="black")
        #d.rectangle([30, 30, 33, 33],fill="blue")
        i.save(open(fname, "wb"), "PNG")

def wrap_q(s):
    return "\"" + s + "\""

def create_mapping(i, s):
    outp = wrap_q(str(i)) + ":" + wrap_q(s)
    return outp



def create_all():
    s = ""
    s += "{\"w\":" + str(box[0]) +","
    s += "\"h\":" + str(box[1]) +","
    s += "\"dict\":{"
    f = ImageFont.truetype("proggy.ttf", 24)
    for i,[g,n] in enumerate(glyphs):
        create_letter(i,g,f)
        s += create_mapping(i,n) + ","
    s += "}}"
    return s

def delete_all():
    for i,[g,n] in enumerate(glyphs):
        try:
            os.remove(i_name(i,g))
        except OSError:
            pass

def usage():
    print "Usage: either gen or clean as first argument"



if __name__ == '__main__':
    total = len(sys.argv)
    sane_fill = int(math.ceil(math.log(len(glyphs), 10)))
    if total != 2:
        usage()
    else:
        if sys.argv[1] == "gen":
            print create_all()
        elif sys.argv[1] == "clean":
            delete_all()
        else:
            usage()
