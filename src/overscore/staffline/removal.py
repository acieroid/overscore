import sys
from gamera.core import *
from gamera.toolkits.musicstaves import *

# We use Roach and Tatem's method to remove staffline (more accurate
# than the other methods available)
method = musicstaves_rl_roach_tatem.MusicStaves_rl_roach_tatem

def remove_stafflines(img_in, img_out, pos_out):
    # Initialize Gamera and load the image
    init_gamera()
    image = load_image(img_in)

    # Remove the stafflines
    ms = method(image)
    ms.remove_staves(crossing_symbols = 'bars')

    # Save the output image
    ms.image.save_PNG(img_out)

    # Save the stafflines positions, in a clojure-readable format
    staves = ms.get_staffpos()
    with open(pos_out, 'w') as f:
        f.write('[')
        # We should only have one system, so we only handle the first one
        # TODO: there seems to be a constant error (around 10px) for each position
        if len(staves) > 0:
            f.write(' '.join(map(str, staves[0].yposlist)))
        f.write(']')

if __name__ == "__main__":
    if len(sys.argv) != 4:
        print("{}: takes 3 arguments, {} given".format(sys.argv[0], len(sys.argv)-1))
        print("Usage: {} img_in img_out pos_out".format(sys.argv[0]))
    else:
        remove_stafflines(sys.argv[1], sys.argv[2], sys.argv[3])
