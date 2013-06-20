#!/usr/bin/python
import Image, ImageDraw, ImageFont, os, sys, math

glyphs = [
    ("g","grass"), #grass
    ("d","dirt"), #dirt
    ("s","snow"), #snow

    # buildings
    ("S-tru","spawn-truck"),
    ("S-tra","spawn-train"),
    ("S-b","spawn-boat"),
    ("Su-r","supply-red"),
    ("Su-g","supply-green"),
    ("Su-b","supply-blue"),
    ("D-r","depot-red"),
    ("D-g","depot-green"),
    ("D-b","depot-blue"),

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
    ("", "default")
    ]

sane_fill = 2

def i_name(i, g):
    fmt = ("%0" + str(sane_fill) +"d")
    return "part-" + fmt % i + g + ".png"

def create_letter(p,g):
    i = Image.new("RGB", (64,64), "magenta")
    d = ImageDraw.Draw(i)
    mask=Image.new('L', i.size, color=255)
    alpha_draw=ImageDraw.Draw(mask)
    alpha_draw.rectangle(i.getbbox(), fill=0)
    i.putalpha(mask)
    f = ImageFont.truetype("/usr/share/vlc/skins2/fonts/FreeSans.ttf", 32)
    d.text((0,0),g, font=f, fill="black")
    i.save(open(i_name(p,g), "wb"), "PNG")

def wrap_q(s):
    return "\"" + s + "\""

def create_mapping(i, s):
    outp = wrap_q(str(i+1)) + ":" + wrap_q(s)
    return outp



def create_all():
    s = ""
    for i,[g,n] in enumerate(glyphs):
        create_letter(i,g)
        s += create_mapping(i,n) + ","
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
