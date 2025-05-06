	program fixed_input_cohort_model

    implicit none

!!############################################################################
!#    Copyright (C) 2024 by Astley Hastings and Jon Reid,                   #
!#    University Aberdeen                                                   #
!#    astley.hastings@abdn.ac.uk                                            #
!#                                                                          #
!#    This program is free software; you can redistribute it and/or modify  #
!#    it under the terms of the GNU General Public License as published by  #
!#    the Free Software Foundation; either version 2 of the License, or     #
!#    (at your option) any later version.                                   #
!#                                                                          #
!#    This program is distributed in the hope that it will be useful,       #
!#    but WITHOUT ANY WARRANTY; without even the implied warranty of        #
!#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         #
!#    GNU General Public License for more details.                          #
!#                                                                          #
!#    You should have received a copy of the GNU General Public License     #
!#    along with this program; if not, write to the                         #
!#    Free Software Foundation, Inc.,                                       #
!#    OR SEE http://www.gnu.org/copyleft/gpl.html                           #
!#    59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             #
!############################################################################
!****************************************************************************************************************************
!
!  PROGRAM: Fixed Input Cohort Model
!           Developed from the program LucFor, by Jon Reid and Astley Hastings copyright reserved.
!           If this software or its derivatives are used in a publication it should be referenced as follows. 
!           Reid J, Hastings A,  xxxxxxx
!
!  PURPOSE: This program enables an analysis of different land use scenarios to be made to determine the changes in 
!           soil organic carbon and live biomass that result from the land use change over time. It also considers
!           the carbon stored in any product derived from biomass be it bioenergy, BECCS or wood, biochar or bioplastic
!           products.
!
!******************************************************************************************************************************

        ! file management variables
        integer:: fl                ! file length
        character(1):: sl           ! OS file separator    
        logical:: dir_e             ! test if a folder exists
        character(1):: noch(2)      ! used for naming of subfolders
        character(22):: f_path      ! path for output variables

        ! indices
        integer:: gp ! grid point index
        integer:: yr ! year index
        integer:: i  ! file creation index
        integer:: n	 ! main do loop index

        ! calculation terms
        real:: pct					! counter used for outputting program progress (has to be real to allow division)
        real:: pct_tot				! maximum value p can reach, used to calculate percentage of program completed
        integer:: qct				! store of what percentage has been written out as progress so far
        integer:: rct				! percentage progress increment to write out
        real:: sct					! temporary store of progress completed        
        
        ! Variables
        character(50):: dummy
        character(len = 150):: header
        integer:: counter, ios
        integer:: x_bng, y_bng
		character(10):: socini_string
		real:: socini
        real:: gs = 1000.		   ! grid size (1km OSGB36 grid) - must use decimal points, else assumes integer result
		
        ! bounds
        real:: xmin = 0				! Minimum Northing
        real:: xmax = 700000		! Maximum Northing
        real:: ymin = 0				! Minimum Easting
        real:: ymax = 1300000		! Maximum Easting
        
        ! RCP scenario variables
        !character(3):: rcp_scenario             ! 2.6, 4.5, 6.0, or 8.5.
        !character(2):: rcp_scenario_ensemble    ! 1, 4, 6, or 15.
		
		character(20):: tree_species
		character(25):: CEH_LCM
		real:: tree_carbon(56)
		real:: plant_input(56)
        
		! cohort calculation variables		
        real:: SOC_year(56)
        real:: initial_SOC_decrease(56)
        real:: initial_soil_carbon_left
		
        real:: litter_model(56)
        real:: annual_SOC(56)
        real:: added_SOC(56)
		
		real:: cumulative_tree_C(56)
		
		!************************************************INPUT X AND Y COORDINATES FOR TIME SERIES GRAPH HERE*************************************************
		integer:: chosen_x_point = 375500
		integer:: chosen_y_point = 1054500
		!*****************************************************************************************************************************************************

        ! \ or / for windows or linux
        open(1,file='os_path.txt',action='read')
        read(1,'(a1)') sl
        close(1)  
		
		! create output folder
		!call make_folder('..'//sl//'..'//sl//'output',12,dir_e)	! dir_e is a dummy here
		!dir_e = .true.	! make true for while loop to work
		!i = 0
		!do while (dir_e)
			!if (i==100) call halt('Program failed - cannot have more than 99 subfolders in output folder',70)
			!i = i+1
			!noch = set_no(i)
			!call make_folder(".."//sl//".."//sl//"output"//sl//"result"//noch(1)//""//noch(2),21,dir_e)
			!call make_folder(".."//sl//".."//sl//"output"//sl//"result"//noch(1)//""//noch(2)//""//sl//"graphs",28,dir_e)
		!end do
		!f_path = ".."//sl//".."//sl//"output"//sl//"result"//noch(1)//""//noch(2)//""//sl
		!write(*,'(a)') 'Results will be written to folder output'//sl//'result'//noch(1)//noch(2)

        ! Read all the parameters from the parameter file
        !OPEN(10, FILE = ".."//sl//".."//sl//"input"//sl//"parameters.txt", ACTION = 'READ') ! format is changed to line by line to add variables
		
        ! variables for potential NPP actually grown. Miami NPP is above ground biomass @ 50% carbon but also consider below=above ground biomass. so NPP is total carbon
        !OPEN(10, FILE ="parameters.txt", ACTION= 'READ') ! format is changed to line by line to add variables
        !read (10, * ) peat       ! soil carbon limit for peat ie no planting.  
        !read (10, * ) rcp_scenario          ! Selected RCP Scenario
        !read (10, * ) rcp_scenario_ensemble ! Selected ensemble of RCP scenario
 
		Open (20, FILE = "Initial_SOC.csv",ACTION = 'write')
		
        Open (23, FILE = "absolute_SOC_2050.csv",ACTION = 'write')
        Open (24, FILE = "delta_SOC_2050.csv",ACTION = 'write')
		
        Open (25, FILE = "tree_carbon_2050.csv",ACTION = 'write')
        Open (26, FILE = "cumulative_tree_carbon_2050.csv",ACTION = 'write')
				
        Open (27, FILE = "absolute_SOC_2080.csv",ACTION = 'write')
        Open (28, FILE = "delta_SOC_2080.csv",ACTION = 'write')
		
        Open (29, FILE = "tree_carbon_2080.csv",ACTION = 'write')
        Open (30, FILE = "cumulative_tree_carbon_2080.csv",ACTION = 'write')
  
        Open (60, FILE = "grid_point_time_series.csv", ACTION = 'write')  
        
        ! Write headers
        !write(41,"(a13,a1,a13,a1,a10,a1,a4,a1,a10,a1,a10,a1,a10,a1,a10,a1,a10,a1,a10,a1,a10,a1,a10)") "x_bng",",","y_bng",",","lusi",",","NPP",",","socini",",","socfinal1",",","socfinal2",",","woodinc1",",","woodinc2",",","woodstore1",",","woodstore2"
		write(60, "(a3,a1,a10,a1,a17)") "soc",",","soc+cohort",",","cumulative_tree_c"
        
        ! open user-selected rcp scenario dataset, combining HILDA land use data, SOC data, and CHESS-SCAPE precipitation & temperature data
        !open (13, FILE = "01_all_species_rcp"//rcp_scenario//"_"//rcp_scenario_ensemble//"_soc_lu.csv", action = 'read')
		open(13, FILE = "01_all_species_rcp26_cumul_treeC_annual_soil-input_Mg_C_ha.csv", action = 'read')

		fl = find_length(13) - 1		! subtract 1 to account for header line
		read(13, *, iostat=ios) header  ! read in header line
		
        pct=0;qct=0;rct=5;pct_tot=fl 
        
        ! step through grid points to find soil parameters and write to file.
        print *, "Running Fixed Input Cohort Model..."
        do n = 1, fl

            ! read in input data
            read (13,*) dummy, x_bng, y_bng, CEH_LCM, socini_string, dummy, dummy, dummy, dummy, tree_species, dummy, tree_carbon, plant_input
			
			! read in initial SOC value as a string due to presence of "NA" in the data, then read that value into a real variable if it is not "NA".
			if (trim(socini_string) /= "NA") then
				read(socini_string,*) socini
			else
				socini = 0;
			end if
            
            ! test if inside bounds
	        if (x_bng<=xmin .or. x_bng>xmax .or. y_bng<=ymin .or. y_bng>ymax) cycle 

            ! write calculation progress to console
            pct = pct + 1
            sct = 100 * pct / pct_tot
            if (sct >= qct + rct) then	! output calculation progress
                write(*,'(i3,a)') IFIX(sct),"% complete"
                qct = sct
			end if
			
			! Initialize arrays for each grid point so they are empty from previous grid point
			SOC_year = 0.0
			initial_SOC_decrease = 0.0
			litter_model = 0.0
			annual_SOC = 0.0
			added_SOC = 0.0
			cumulative_tree_C = 0.0
            
			do yr = 1, 56	! runs for 56 years, 2025-2080 inclusive
				
                ! cohort model calcs - replicating Hastings Cohort Model spreadsheet
                initial_soil_carbon_left = 0.0 * EXP(-(yr - 1.0) / 1.0) + 0.0 * EXP(-(yr - 1.0) / 2.0) + 0.44 * EXP(-(yr - 1.0) / 30.0) + 0.56 * EXP(-(yr - 1.0) / 500.0) ! Column L in spreadsheet
                SOC_year(yr) = socini * initial_soil_carbon_left ! Column N in spreadsheet - this is the lower line on the graph.
				initial_SOC_decrease(yr) = socini - SOC_year(yr) ! Column I in spreadsheet
			  
				! *** Litter model ***
                litter_model(yr) = plant_input(yr) * (0.48 * EXP(-(yr - 1.0) / 0.1) + 0.47 * EXP(-(yr - 1.0) / 30.0) + 0.05 * EXP(-(yr - 1.0) / 500.0)) ! Column Q in spreadsheet
                annual_SOC(yr) = SOC_year(yr) + SUM(litter_model) - litter_model(yr) ! Column H in spreadsheet - this is the middle line on the graph.
                added_SOC(yr) = annual_SOC(yr) - SOC_year(yr) ! Column J in spreadsheet
				
				! Upper line on graph - given tree carbon values + the middle line on the graph.
				cumulative_tree_C(yr) = tree_carbon(yr) + annual_SOC(yr)
				
				! write time series CSV file for selected grid point
				if (x_bng == chosen_x_point .and. y_bng == chosen_y_point) then
					write(60,"(F15.5,a1,F15.5,a1,F15.5,a1,F15.5,a1,F15.5,a1,F15.5,a1,F15.5)") SOC_year(yr),',',annual_SOC(yr),',',cumulative_tree_C(yr)	! time series graph of selected grid point
				end if
								
                ! first write files at year 2050
                if (yr .eq. 26) then 					
					write(23,"(i10,a1,i10,a1,F15.5)") x_bng,',',y_bng,',',annual_SOC(yr)				! The absolute value of SOC after the defined period ** column H from spreadsheet ** middle line on graph
					write(24,"(i10,a1,i10,a1,F15.5)") x_bng,',',y_bng,',',(annual_SOC(yr) - socini)		! The delta value between the final SOC and the initial SOC 
					
					write(25,"(i10,a1,i10,a1,F15.5)") x_bng,',',y_bng,',',tree_carbon(yr)					! Tree carbon values inside input spreadsheet
					write(26,"(i10,a1,i10,a1,F15.5)") x_bng,',',y_bng,',',cumulative_tree_C(yr)				! Tree carbon + SOC ** upper line on graph
					
				end if
				
				! then write files at year 2080
				if (yr .eq. 56) then 
					write(20,"(i10,a1,i10,a1,F15.5)") x_bng,',',y_bng,',',socini					! Initial SOC
					
					!write(21,"(i10,a1,i10,a1,F15.5)") x_bng,',',y_bng,',',SOC_year(yr)				! The absolute value of SOC after the defined period ** column N from spreadsheet ** lower line on SOC change graph
					!write(22,"(i10,a1,i10,a1,F15.5)") x_bng,',',y_bng,',',initial_SOC_decrease(yr)	! The delta value of SOC between the final SOC and the initial SOC ** column I from spreadsheet ** lower line on SOC change graph
					
					write(27,"(i10,a1,i10,a1,F15.5)") x_bng,',',y_bng,',',annual_SOC(yr)				! The absolute value of SOC after the defined period ** column H from spreadsheet ** middle line on graph
					write(28,"(i10,a1,i10,a1,F15.5)") x_bng,',',y_bng,',',(annual_SOC(yr) - socini)		! The delta value between the final SOC and the initial SOC 
					
					write(29,"(i10,a1,i10,a1,F15.5)") x_bng,',',y_bng,',',tree_carbon(yr)					! Tree carbon values inside input spreadsheet
					write(30,"(i10,a1,i10,a1,F15.5)") x_bng,',',y_bng,',',cumulative_tree_C(yr)				! Tree carbon + SOC ** upper line on graph
					!write(35,"(i10,a1,i10,a1,F15.13)") x_bng,',',y_bng,',',litter_model(yr)				! Column Q from spreadsheet
					
                end if
            end do
		end do
		
		! Close all files
		close(13); close(20); close(23); close(24); close(25); close(26)
		close(27); close(28); close(29); close(30); close(60)
		
        ! plot sample graphs with python subroutine
		print *, "Plotting sample graphs..."
        call write_batch_plot_script()
        call system("python batch_plot.py")
		call write_python_plot("grid_point_time_series.csv", chosen_x_point, chosen_y_point)

        print *, "finitio la musica"
        write(*,*) 'Press enter to continue',n
        read(*,*)	! require this to keep dos prompt open in .exe version!

        !--------------------------------------------------
        contains   
        !--------------------------------------------------
        function find_length(f_no)	! finds length of file, avoiding eof() function which doesn't work on long files
        integer:: f_no, find_length
        find_length = 0
        rewind(f_no)	!sets to start of file
        do while (.true.)
            read(f_no,*,end=999)
            find_length = find_length + 1
        end do
        999 rewind(f_no)
        return
        end function
        ! -------------------------------------------------
        subroutine make_folder(str,le,su)
        implicit none
        character(*):: str
        integer:: le
        logical:: su
        inquire(directory="./"//str(1:le)//"/.", exist=su)	! windows compaq compiler accepts this
        !inquire(directory=''//str(1:le)//'', exist=su)	! linux intel compiler accepts this
        if (su==.false.) then
            call system("mkdir "//str(1:le)//"")
        end if
        return
        end subroutine
        ! -------------------------------------------------
        function set_no(j)	! make string (actually array) of number j, of length 2 and padded with 0 if less than 10
        ! NB - set_no is a character array, not a string
        implicit none
        integer:: j, tens, units
        character(1):: set_no(2)
        if (j<10) then	! set number to 2 digit string
            write(set_no(1),'(i1)') 0
            write(set_no(2),'(i1)') j
        else if (j<100) then
            tens = ifix(real(j)/10)
            units = j - 10 * tens
            write(set_no(1),'(i1)') tens
            write(set_no(2),'(i1)')	units
        end if
        return
		end function
		! -------------------------------------------------
		subroutine halt(str,l)	! end program
		implicit none
		integer:: l
		character(l):: str
		write(*,'(a)') str
		write(*,'(a)') 'Press enter to continue'
		read(*,*)
		stop
		end subroutine
		!----------------------------------------------------------------
		subroutine write_batch_plot_script()
		implicit none
		integer :: unit
		unit = 90
		open(unit, file="batch_plot.py", action="write", status="replace")

		write(unit,'(a)') "import numpy as np"
		write(unit,'(a)') "import matplotlib.pyplot as plt"
		write(unit,'(a)') "from matplotlib.ticker import FuncFormatter"
		write(unit,'(a)') "files = ["
		write(unit,'(a)') "    ('Initial_SOC.csv', 'Initial SOC'),"
		write(unit,'(a)') "    ('absolute_SOC_2050.csv', 'Absolute SOC 2050'),"
		write(unit,'(a)') "    ('absolute_SOC_2080.csv', 'Absolute SOC 2080'),"
		write(unit,'(a)') "    ('delta_SOC_2050.csv', 'Delta SOC 2050'),"
		write(unit,'(a)') "    ('delta_SOC_2080.csv', 'Delta SOC 2080'),"
		write(unit,'(a)') "    ('tree_carbon_2050.csv', 'Tree Carbon 2050'),"
		write(unit,'(a)') "    ('tree_carbon_2080.csv', 'Tree Carbon 2080'),"
		write(unit,'(a)') "    ('cumulative_tree_carbon_2050.csv', 'Cumulative Tree Carbon 2050'),"
		write(unit,'(a)') "    ('cumulative_tree_carbon_2080.csv', 'Cumulative Tree Carbon 2080'),"
		write(unit,'(a)') "]"
		write(unit,'(a)') ""
		write(unit,'(a)') "for fname, title in files:"
		write(unit,'(a)') "    data = np.loadtxt(fname, delimiter=',')"
		write(unit,'(a)') "    x = data[:,0]; y = data[:,1]; vals = data[:,2]"
		write(unit,'(a)') "    sf = 1000.0"
		write(unit,'(a)') "    grid_shape = (1+int((y.max()-y.min())/sf), 1+int((x.max()-x.min())/sf))"
		write(unit,'(a)') "    grid = np.full(grid_shape, np.nan)"
		write(unit,'(a)') "    yi = ((y-y.min())/sf).astype(int)"
		write(unit,'(a)') "    xi = ((x-x.min())/sf).astype(int)"
		write(unit,'(a)') "    np.clip(yi, 0, grid_shape[0]-1, out=yi)"
		write(unit,'(a)') "    np.clip(xi, 0, grid_shape[1]-1, out=xi)"
		write(unit,'(a)') "    grid[yi, xi] = vals"
		write(unit,'(a)') "    fig, ax = plt.subplots()"
		write(unit,'(a)') "    ax.set_axis_off()"
		write(unit,'(a)') "    im = ax.imshow(np.ma.masked_invalid(grid), origin='lower', interpolation='nearest', cmap='gist_ncar')"
		write(unit,'(a)') "    cbar = plt.colorbar(im, shrink=0.5)"
		write(unit,'(a)') "    ax.set_title(title)"
		write(unit,'(a)') "    cbar.set_label('SOC (t C ha$^{-1}$)', rotation=270, labelpad=15)"
		write(unit,'(a)') "    ax.xaxis.set_major_formatter(FuncFormatter(lambda x, _: f'{int(x)}'))"
		write(unit,'(a)') "    ax.yaxis.set_major_formatter(FuncFormatter(lambda y, _: f'{int(y)}'))"
		write(unit,'(a)') "    out = title.replace(' ','_') + '.png'"
		write(unit,'(a)') "    fig.savefig(out, dpi=500, bbox_inches='tight')"
		write(unit,'(a)') "    plt.close(fig)"

		close(unit)
		end subroutine write_batch_plot_script
        ! -------------------------------------------------
		subroutine write_python_plot(input_csv, x, y)
		
		implicit none

		! Inputs:
		character(len=*), intent(in) :: input_csv        ! Path to the 3-column CSV file
		integer:: x, y						 ! Coordinates for the title of the plot
		character(20):: x_str, y_str
		
		write(x_str, '(i10)') x
		write(y_str, '(i10)') y	

		! Open the Python script for writing (overwrite if it exists)
		open(unit=70, file="plotgraph.py", action='write')

		! Write the Python plotting code
		write(70, '(A)') "import pandas as pd"
		write(70, '(A)') "import matplotlib.pyplot as plt"
		write(70, '(A)') ""
		write(70, '(A)') "# Read the CSV file"
		write(70, '(A)') "df = pd.read_csv(r'" // trim(input_csv) // "')"
		write(70, '(A)') ""
		write(70, '(A)') "# Generate years from 2025 to 2080"
		write(70, '(A)') "years = list(range(2025, 2081))"
		write(70, '(A)') ""
		write(70, '(A)') "# Plot each SOC series against Year"
		write(70, '(A)') "plt.figure()"
		write(70, '(A)') "for col in df.columns:"
		write(70, '(A)') "    plt.plot(years, df[col], label=col)"
		write(70, '(A)') ""
		write(70, '(A)') "plt.xlabel('Year')"
		write(70, '(A)') "plt.ylabel('SOC (t C ha$^{-1}$)')"
		write(70, '(A)') "plt.title('Grid Point "//trim(adjustl(x_str))//" "//trim(adjustl(y_str))//" Time Series 2025-2080')"
		write(70, '(A)') "plt.legend()"
		write(70, '(A)') "plt.xlim(2025, 2080)"
		write(70, '(A)') "plt.savefig('Grid_Point_"//trim(adjustl(x_str))//"_"//trim(adjustl(y_str))//"_Time_Series.png')"

		close(70)
        call system("python plotgraph.py")
		end subroutine write_python_plot
        ! -------------------------------------------------

		end program fixed_input_cohort_model

