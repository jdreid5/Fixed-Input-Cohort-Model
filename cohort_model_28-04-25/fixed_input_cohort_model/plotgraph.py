import pandas as pd
import matplotlib.pyplot as plt

# Read the CSV file
df = pd.read_csv(r'grid_point_time_series.csv')

# Generate years from 2025 to 2124
years = list(range(2025, 2125))

# Plot each SOC series against Year
plt.figure()
for col in df.columns:
    plt.plot(years, df[col], label=col)

plt.xlabel('Year')
plt.ylabel('SOC (t C ha$^{-1}$)')
plt.title('Grid Point 375500 1054500 Time Series 2025-2124')
plt.legend()
plt.xlim(2025, 2124)
plt.savefig('Grid_Point_375500_1054500_Time_Series.png')
