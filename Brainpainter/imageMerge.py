import os
import numpy as np
import PIL
from PIL import Image, ImageDraw, ImageFont

os.chdir("E:/Users/spectR/Desktop/%Rutgers/RESEARCH/brain-coloring/output/DK_output")

#merge left regions horizontally
#list_im_left = ['cortical-inner_left_area.png', 'cortical-inner_left_grayvol.png', 'cortical-inner_left_thck.png',\
#    'cortical-outer_left_area.png', 'cortical-outer_left_grayvol.png', 'cortical-outer_left_thck.png',\
#    'subcortical_left_area.png']
list_im_left = ['cortical-inner_left_area.png', 'cortical-outer_left_area.png', 'cortical-inner_left_thck.png',\
    'cortical-outer_left_thck.png', 'cortical-inner_left_grayvol.png', 'cortical-outer_left_grayvol.png',\
    'subcortical_left_area.png']

ind_size = Image.open(list_im_left[1]).size

imgs_left = [ PIL.Image.open(i) for i in list_im_left ]

min_shape = sorted( [(np.sum(i.size), i.size ) for i in imgs_left])[0][1]
imgs_comb = np.hstack( (np.asarray( i.resize(min_shape) ) for i in imgs_left ) )

imgs_comb = PIL.Image.fromarray( imgs_comb)
imgs_comb.save( 'left.png' )

#merge right regions horizontally
#list_im_right = ['cortical-inner_right_area.png', 'cortical-inner_right_grayvol.png', 'cortical-inner_right_thck.png',\
#    'cortical-outer_right_area.png', 'cortical-outer_right_grayvol.png', 'cortical-outer_right_thck.png',\
#    'subcortical_right_area.png']
list_im_right = ['cortical-inner_right_area.png', 'cortical-outer_right_area.png', 'cortical-inner_right_thck.png',\
    'cortical-outer_right_thck.png', 'cortical-inner_right_grayvol.png', 'cortical-outer_right_grayvol.png',\
    'subcortical_right_area.png']

imgs_right = [ PIL.Image.open(i) for i in list_im_right ]

min_shape = sorted( [(np.sum(i.size), i.size ) for i in imgs_right])[0][1]
imgs_comb = np.hstack( (np.asarray( i.resize(min_shape) ) for i in imgs_right ) )

imgs_comb = PIL.Image.fromarray( imgs_comb)
imgs_comb.save( 'right.png' )

#merge left and right vertically
left = Image.open('left.png')
right = Image.open('right.png')

left_size = left.size
right_size = right.size

new_im = Image.new('RGB', (left_size[0],right_size[1] + left_size[1]+200), (255,255,255))
new_im.paste(left,(0,100))
new_im.paste(right,(0,left_size[1] + 200))

new_im.save('merge.png')


#add captions
img = Image.open('merge.png')
draw = ImageDraw.Draw(img,'RGB')
font = ImageFont.truetype("arial.ttf", 50)

left_names = ['left cortical-inner area', 'left cortical-outer area', 'left cortical-inner thickness',\
    'left cortical-outer thickness', 'left cortical-inner gray matter volume', 'left cortical-outer gray matter volume',\
    'left subcortical volume']
for i in range(7):
    draw.text(((int)(ind_size[0]/2 + i*ind_size[0]-240),75), left_names[i], (0, 0, 0),font=font)
    #print(left_names[i])
    #print(ind_size[0]/2 + i*ind_size[0])
    #print(left_size[1])

right_names = ['right cortical-inner area', 'right cortical-outer area', 'right cortical-inner thickness',\
    'right cortical-outer thickness', 'right cortical-inner gray matter volume', 'right cortical-outer gray matter volume',\
    'right subcortical volume']
for i in range(7):
    draw.text(((int)(ind_size[0]/2 + i*ind_size[0]-240),left_size[1]+175), right_names[i], (0, 0, 0),font=font)

img.save('merge.png')

#list_im = ['left.png','right.png']
#imgs    = [ PIL.Image.open(i) for i in list_im ]

#min_shape = sorted( [(np.sum(i.size), i.size ) for i in imgs])[0][1]
#imgs_comb = np.vstack( (np.asarray( i.resize(min_shape) ) for i in imgs ) )

#imgs_comb = PIL.Image.fromarray( imgs_comb)
#imgs_comb.save( 'merge.png' )


#merge cold and warm color scale horizontally
cold = Image.open('cold2.png')
warm = Image.open('warm2.png')

cold_size = cold.size

new_im = Image.new('RGBA', (2*cold_size[0], cold_size[1]), (255,255,255,0))
new_im.paste(cold, (0,0))
new_im.paste(warm, (cold_size[0],0))

new_im.save('cold_warm.png')


#list_im_colorscale = ['cold3.png','warm3.png']
#imgs_color = [ PIL.Image.open(i) for i in list_im_colorscale ]

#min_shape = sorted( [(np.sum(i.size), i.size ) for i in imgs_color])[0][1]
#imgs_comb = np.vstack( (np.asarray( i.resize(min_shape) ) for i in imgs_color ) )

#imgs_comb = PIL.Image.fromarray( imgs_comb)
#imgs_comb.save( 'coldwarm.png' )


#merge brain regions and color scale vertically
merge = Image.open( 'merge.png' )
scale = Image.open('cold_warm.png')

maxsize = (2800, 600)
merge.thumbnail(maxsize, PIL.Image.ANTIALIAS)

merge_size = merge.size
scale_size = scale.size

new_im_final = Image.new('RGB', (merge_size[0],merge_size[1]+scale_size[1]), (255,255,255))
new_im_final.paste(merge,(0,0))
new_im_final.paste(scale, ((int)((merge_size[0]/2) - (scale_size[0]/2)), merge_size[1]))

new_im_final.save('final.png')







#list_im_final = ['merge.png','cold_warm.png']
#imgs    = [ PIL.Image.open(i) for i in list_im ]

#min_shape = sorted( [(np.sum(i.size), i.size ) for i in imgs])[0][1]
#imgs_comb = np.vstack( (np.asarray( i.resize(min_shape) ) for i in imgs ) )

#imgs_comb = PIL.Image.fromarray( imgs_comb)
#imgs_comb.save( 'final.png' )


