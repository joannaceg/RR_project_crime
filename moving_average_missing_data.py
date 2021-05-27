import pandas as pd

def interpolate_ma(df, window):
    for column in df:
        nans = list(df[column].isna())
        values = list(df[column])
        if sum(nans) != 0:
            for i, val in enumerate(nans):
                if i == 0 and val:
                    values[i] = values[min([i for i, x in enumerate(nans) if not x])]
                elif i > 0 and i < window and val:
                    values[i] = np.mean(values[0:i])
                elif i >= window and val:
                    values[i] = np.mean(values[i-window:i])

        df[column] = values
                
    return df
