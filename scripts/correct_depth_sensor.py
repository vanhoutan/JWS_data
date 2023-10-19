import numpy as np
from scipy.interpolate import PchipInterpolator, interp1d
import matplotlib.pyplot as plt
import pandas, datetime, os

#Hi Kisei, Here is the important file directory with the scripts for tag data corrections.
#C:\A_MBAq\PAPER Climate refugia\CR_databases\124\JWS_PAT_Data\WCH_Exported


# def get_dy(y):
#     return min([dyi for dyi in np.diff(y) if dyi > 0.])

# def correct_data_hist(time, depth, threshold=5):
    
#     # Get bounds of each day
#     n = len(depth)
#     day_idx = [0]+[i for i, diffi in enumerate(np.diff([timei.day for timei in time])) if diffi>0.]+[n]
#     n_day   = len(day_idx)

#     # Get min depth for each day
#     for i in range(1, n_day):
#         # for i in [1]:
#         idx = range(day_idx[i-1], day_idx[i])

#         depth_max = max(depth[idx])
#         depth_min = min(depth[idx])
#         dy        = get_dy(depth)

#         bins = np.linspace(depth_min-dy,depth_max+dy,int((depth_max-depth_min)/dy+3))
#         bins = (bins[1:] + bins[:-1]) / 2
#         bins_mid = np.linspace(depth_min,depth_max,int((depth_max-depth_min)/dy+1))

#         depth_hist, bins2 = np.histogram(depth, bins)

#         depth_error = [max([depthi for depthi, counti in zip(bins_mid[::-1], depth_hist[::-1]) if counti>threshold]) for i in idx]


#         plt.plot(depth)
#         plt.plot(idx, depth_error)
#         plt.show()

#         # exit()


#     return depth_corrected


def correct_data(time, depth):
    
    # Get bounds of each day
    n = len(depth)
    day_idx = [0]+[i for i, diffi in enumerate(np.diff([timei.day for timei in time])) if diffi>0.]+[n]
    n_day   = len(day_idx)

    # Get min depth for each day
    max_depth_error = []
    depth_corrected = []
    for i in range(1, n_day):
        depth_dayi = np.array(depth[day_idx[i-1]:day_idx[i]])

        max_depth_error.extend([-max(depth_dayi)]*len(depth_dayi))
        depth_corrected.extend(depth_dayi + max_depth_error[day_idx[i-1]:day_idx[i]])

    return depth_corrected
    


def process_csv(fname_in):

    df = pandas.read_csv(fname_in)

    # Find non-n/a's
    nan_check = df['Depth'].notna()
    n = sum(np.array(nan_check))

    # Get time and depth
    time  = [datetime.datetime.strptime(timei, '%Y-%m-%d %H:%M:%S') for timei, checki in zip(df['Time'], nan_check) if checki]
    depth = np.array([-depthi for depthi, checki in zip(df['Depth'], nan_check) if checki])

    # Correct depths
    # depth_corrected = correct_data_hist(time, depth)
    depth_corrected = correct_data(time, depth)

    # Store corrected
    count = 0
    for i, checki in enumerate(nan_check):
        if checki:
            # df['Depth'][i] = -depth_corrected[count]
            df.at[i, 'Depth'] = -depth_corrected[count]
            count += 1

    # Save corrected
    fname = '.'.join(fname_in.split('.')[:-1]) + '_CD.csv'
    df.to_csv(fname, index = False)


if __name__ == "__main__":


    ### option 1: list specific input files: ###
     flist = ["C:/Users/bdias/Dropbox/A_MBAq/PAPER Climate refugia/CR_databases/124/JWS_PAT_Data/WCH_Exported/wch_archive/JWS_08_04_06A1312_64252-Archive.csv"]

    # flist = [
    #          "data/JWS_08_11_07A0930_83060-Archive.csv",
    #          "data/JWS_08_09_07A0937_83066-Archive.csv",
    #          "data/JWS_09_12_08A0710_88761-Archive.csv",
    #          "data/JWS_09_15_08A0711_88762-Archive.csv",
    #          "data/JWS_10_05_08A0710_88761-Archive.csv",
    #          "data/JWS_10_07_08A0688_88772-Archive.csv",
    #          "data/JWS_10_10_08A0710_88761-Archive.csv",
    #          "data/JWS_10_19_08A0716_88767-Archive.csv",
    #          "data/JWS_02_01_01P0061_18616-Archive.csv",
    #          "data/JWS_06_10_05A0376_40564-Archive.csv",
    #          "data/JWS_07_01_06A0531_64272-Archive.csv",
    #          "data/JWS_07_05_06A0928_66885-Archive.csv",
    #          "data/JWS_08_01_05A0385_49561-Archive.csv",
    #          "data/JWS_08_02_05A0378_55716-Archive.csv",
    #          "data/JWS_08_04_06A1312_64252-Archive.csv"]

    ### option 2: get all input files in a directory ###
   # file_directory =  'C:/Users/bdias/Dropbox/A_MBAq/PAPER Climate refugia/CR_databases/124/JWS_PAT_Data/WCH_Exported/wch_archive/JWS_07_01_06A0531_64272-Archive.csv" '
    #fnames_include = '-Archive.csv'
   # flist = [os.path.join(file_directory, fname) for fname in os.listdir(file_directory) if fnames_include in fname]
    
    # loop through files, save corrected data
for i, fname in enumerate(flist):
        print('%d/%d %s'%(i+1, len(flist), fname))
        process_csv(fname)
