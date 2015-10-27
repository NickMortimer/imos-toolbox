function scatterMooringPlannedDepths(sample_data, isQC, saveToFile, exportDir)
%SCATTERMOORINGPLANNEDDEPTHS Opens a new window where the DEPTH
% variable of the selected intrument is plotted and compared to the
% planned depth.
%
% Inputs:
%   sample_data - cell array of structs containing the entire data set and dimension data.
%
%   varName     - string containing the IMOS code for requested parameter.
%
%   isQC        - logical to plot only good data or not.
%
%   saveToFile  - logical to save the plot on disk or not.
%
%   exportDir   - string containing the destination folder to where the
%               plot is saved on disk.
%
% Author: Rebecca Cowley <rebecca.cowley@csiro.au>
%

%
% Copyright (c) 2009, eMarine Information Infrastructure (eMII) and Integrated 
% Marine Observing System (IMOS).
% All rights reserved.
% 
% Redistribution and use in source and binary forms, with or without 
% modification, are permitted provided that the following conditions are met:
% 
%     * Redistributions of source code must retain the above copyright notice, 
%       this list of conditions and the following disclaimer.
%     * Redistributions in binary form must reproduce the above copyright 
%       notice, this list of conditions and the following disclaimer in the 
%       documentation and/or other materials provided with the distribution.
%     * Neither the name of the eMII/IMOS nor the names of its contributors 
%       may be used to endorse or promote products derived from this software 
%       without specific prior written permission.
% 
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
% POSSIBILITY OF SUCH DAMAGE.
%
error(nargchk(4,4,nargin));

if ~iscell(sample_data),    error('sample_data must be a cell array');  end
if ~islogical(isQC),        error('isQC must be a logical');            end
if ~islogical(saveToFile),  error('saveToFile must be a logical');      end
if ~ischar(exportDir),      error('exportDir must be a string');        end

varTitle = imosParameters('DEPTH', 'long_name');
varUnit = imosParameters('DEPTH', 'uom');

stringQC = 'non QC';
if isQC, stringQC = 'QC'; end

%plot depth information
monitorRec = get(0,'MonitorPosition');
xResolution = monitorRec(:, 3)-monitorRec(:, 1);
iBigMonitor = xResolution == max(xResolution);
if sum(iBigMonitor)==2, iBigMonitor(2) = false; end % in case exactly same monitors

title = [sample_data{1}.deployment_code ' mooring planned depth vs measured depth ' stringQC '''d good ' varTitle];

%sort instruments by depth
lenSampleData = length(sample_data);
metaDepth = nan(lenSampleData, 1);
xMin = nan(lenSampleData, 1);
xMax = nan(lenSampleData, 1);
iPRel = xMax;
for i=1:lenSampleData
    if ~isempty(sample_data{i}.meta.depth)
        metaDepth(i) = sample_data{i}.meta.depth;
    elseif ~isempty(sample_data{i}.instrument_nominal_depth)
        metaDepth(i) = sample_data{i}.instrument_nominal_depth;
    else
        metaDepth(i) = NaN;
    end
    iTime = getVar(sample_data{i}.dimensions, 'TIME');
    %check for pres_rel
    iPRel(i) = getVar(sample_data{i}.variables, 'PRES_REL');
    
    xMin(i) = min(sample_data{i}.dimensions{iTime}.data);
    xMax(i) = max(sample_data{i}.dimensions{iTime}.data);
end
%only look at indexes with pres_rel
[metaDepth, iSort] = sort(metaDepth);
iPRel = iPRel(iSort);
sample_data = sample_data(iSort);
iSort(iPRel==0) = []; 
metaDepth(iPRel==0) = [];
sample_data(iPRel==0) = [];
xMin = min(xMin);
xMax = max(xMax);

%set up some matrices
instrumentDesc = cell(length(metaDepth), 1);
hLineVar = nan(length(metaDepth), 1);

initiateFigure = true;
isPlottable = false;

%color map
cMap = hsv(length(metaDepth));

%now plot all the calculated depths on one plot to choose region for comparison:
for i=1:length(metaDepth)
    % instrument description
    if ~isempty(strtrim(sample_data{(i)}.instrument))
        instrumentDesc{i} = sample_data{(i)}.instrument;
    elseif ~isempty(sample_data{(i)}.toolbox_input_file)
        [~, instrumentDesc{i}] = fileparts(sample_data{(i)}.toolbox_input_file);
    end
    
    instrumentSN = '';
    if ~isempty(strtrim(sample_data{(i)}.instrument_serial_number))
        instrumentSN = [' - ' sample_data{(i)}.instrument_serial_number];
    end
    
    instrumentDesc{i} = [strrep(instrumentDesc{i}, '_', ' ') ' (' num2str(metaDepth((i))) 'm' instrumentSN ')'];
        
    %look for time and relevant variable
    iTime = getVar(sample_data{(i)}.dimensions, 'TIME');
    if isQC
        iVar = getVar(sample_data{(i)}.variables, 'DEPTH');
    else
        iVar = getVar(sample_data{(i)}.variables, 'PRES_REL');
    end
    
    
    if initiateFigure
        %plot
        fileName = genIMOSFileName(sample_data{i}, 'png');
        visible = 'on';
        if saveToFile, visible = 'off'; end
        hFigPress = figure(...
            'Name', title, ...
            'NumberTitle','off', ...
            'Visible', visible, ...
            'OuterPosition', [0, 0, monitorRec(iBigMonitor, 3), monitorRec(iBigMonitor, 4)]);
        
        %add a button to run the region selection:
        zone = uicontrol('Style','pushbutton',...
            'String','Select region',...
            'Parent',hFigPress,...
            'Tag','selectzone');
        
        %depth plot for selecting region to compare depth to planned depth
        hAxPress = axes('Parent', hFigPress);
        set(hAxPress, 'YDir', 'reverse')
        set(get(hAxPress, 'XLabel'), 'String', 'Time');
        set(get(hAxPress, 'YLabel'), 'String', ['DEPTH (' varUnit ')'], 'Interpreter', 'none');
        set(get(hAxPress, 'Title'), 'String', 'Depth', 'Interpreter', 'none');
        set(hAxPress, 'XTick', (xMin:(xMax-xMin)/4:xMax));
        set(hAxPress, 'XLim', [xMin, xMax]);
        hold(hAxPress, 'on');
        
        %now plot the data:
        iGood = true(size(sample_data{(i)}.variables{iVar}.data));
        XLine = sample_data{(i)}.dimensions{iTime}.data;
        XLine(~iGood) = NaN;
        
        dataVar = sample_data{(i)}.variables{iVar}.data;
        dataVar(~iGood) = NaN;
        
        hLineVar(1) = line(XLine, ...
            dataVar, ...
            'Color', cMap(i,:), ...
            'LineStyle', '-',...
            'Parent',hAxPress);
        
        initiateFigure = false;

    end
    iGood = true(size(sample_data{(i)}.variables{iVar}.data));
    
    if isQC
        %get time and var QC information
        timeFlags = sample_data{(i)}.dimensions{iTime}.flags;
        varFlags = sample_data{(i)}.variables{iVar}.flags;
        
        iGood = (timeFlags == 0 | timeFlags == 1 | timeFlags == 2) & (varFlags == 1 | varFlags == 2);
    end
    
    if all(~iGood) && isQC
        fprintf('%s\n', ['Warning : in ' sample_data{(i)}.toolbox_input_file ...
            ', there is not any ' varName ' data with good flags.']);
        continue;
    else
        isPlottable = true;
        
        xLine = sample_data{(i)}.dimensions{iTime}.data;
        xLine(~iGood) = NaN;
        
        dataVar = sample_data{(i)}.variables{iVar}.data;
        if isQC
            %calculate depth
            dataVar = -gsw_z_from_p(dataVar, sample_data{(i)}.latitude);
        end
        dataVar(~iGood) = NaN;
        
        %add to the plot
        hLineVar(i + 1) = line(xLine, ...
            dataVar, ...
            'Color', cMap(i, :), ...
            'LineStyle', '-','Parent',hAxPress);
                        
        % set background to be grey
%         set(hAxPressDiff, 'Color', [0.75 0.75 0.75])
    end
end
% Let's redefine properties after pcolor to make sure grid lines appear
% above color data and XTick and XTickLabel haven't changed
set(hAxPress, ...
    'XTick',        (xMin:(xMax-xMin)/4:xMax), ...
    'XGrid',        'on', ...
    'YGrid',        'on', ...
    'Layer',        'top');

if isPlottable
    iNan = isnan(hLineVar);
    if any(iNan)
        hLineVar(iNan) = [];
        instrumentDesc(iNan) = [];
    end
        
    datetick(hAxPress, 'x', 'dd-mm-yy HH:MM:SS', 'keepticks');
    
    hLegend = legend(hAxPress, ...
        hLineVar,       instrumentDesc, ...
        'Interpreter',  'none', ...
        'Location',     'SouthOutside');
    
    set(hLegend,'Fontsize',14)
end

%Ask for the user to select the region they would like to use for
%comparison
%This could be done better, with more finesse - could allow zooming in
%before going straight to the time period selection. For now, this will do.
helpdlg('Select the time period for comparison using the mouse','Time Period Selection');

zoom off
[x,y] = select_points;


initiateFigure = true;

% Now take the minimum of selected region and compare to planned depths
for i = 1:length(iSort)
    %look for time and relevant variable
    iTime = getVar(sample_data{(i)}.dimensions, 'TIME');
    if isQC
        iVar = getVar(sample_data{(i)}.variables, 'DEPTH');
    else
        iVar = getVar(sample_data{(i)}.variables, 'PRES_REL');
    end
    
    
    if initiateFigure
        %plot
        fileName = genIMOSFileName(sample_data{i}, 'png');
        visible = 'on';
        if saveToFile, visible = 'off'; end
        hFigPress = figure(...
            'Name', title, ...
            'NumberTitle','off', ...
            'Visible', visible, ...
            'OuterPosition', [0, 0, monitorRec(iBigMonitor, 3), monitorRec(iBigMonitor, 4)]);
        
        %planned Depth vs Actual
        hAxPress = subplot(2,1,1,'Parent', hFigPress);
        set(hAxPress, 'YDir', 'reverse')
        set(get(hAxPress, 'XLabel'), 'String', 'Time');
        set(get(hAxPress, 'YLabel'), 'String', ['DEPTH (' varUnit ')'], 'Interpreter', 'none');
        set(get(hAxPress, 'Title'), 'String', 'Depth', 'Interpreter', 'none');
        set(hAxPress, 'XTick', (xMin:(xMax-xMin)/4:xMax));
        set(hAxPress, 'XLim', [xMin, xMax]);
        hold(hAxPress, 'on');
                
        %Actual depth minus planned depth
        hAxPressDiff = subplot(2,1,2,'Parent', hFigPress);
        set(get(hAxPressDiff, 'XLabel'), 'String', 'Time');
        set(get(hAxPressDiff, 'YLabel'), 'String', ['PRES_REL (' varUnit ')'], 'Interpreter', 'none');
        set(get(hAxPressDiff, 'Title'), 'String', ...
            ['Pressure Differences from ' instrumentDesc{1}] , 'Interpreter', 'none');
        set(hAxPressDiff, 'XTick', (xMin:(xMax-xMin)/4:xMax));
        set(hAxPressDiff, 'XLim', [xMin, xMax]);
        hold(hAxPressDiff, 'on');
        
        linkaxes([hAxPressDiff,hAxPress],'x')
        
        %now plot the data:
        iGood = true(size(sample_data{(i)}.variables{iVar}.data));
        XLine = sample_data{(i)}.dimensions{iTime}.data;
        XLine(~iGood) = NaN;
        
        dataVar = sample_data{(i)}.variables{iVar}.data;
        if isQC
            %calculate depth
            dataVar = -gsw_z_from_p(dataVar, sample_data{(i)}.latitude);
        end
        dataVar(~iGood) = NaN;
        
        %Limit to the range selected:
        
        
        hLineVar(1) = line(XLine, ...
            dataVar, ...
            'Color', 'k', ...
            'LineStyle', '-',...
            'Parent',hAxPress);
        
        initiateFigure = false;

    end
    iGood = true(size(sample_data{(i)}.variables{iVar}.data));
    
    if isQC
        %get time and var QC information
        timeFlags = sample_data{(i)}.dimensions{iTime}.flags;
        varFlags = sample_data{(i)}.variables{iVar}.flags;
        
        iGood = (timeFlags == 0 | timeFlags == 1 | timeFlags == 2) & (varFlags == 1 | varFlags == 2);
    end
    
    if all(~iGood) && isQC
        fprintf('%s\n', ['Warning : in ' sample_data{(i)}.toolbox_input_file ...
            ', there is not any ' varName ' data with good flags.']);
        continue;
    else
        isPlottable = true;
        
        xLine = sample_data{(i)}.dimensions{iTime}.data;
        xLine(~iGood) = NaN;
        
        dataVar = sample_data{(i)}.variables{iVar}.data;
        if isQC
            %calculate depth
            dataVar = -gsw_z_from_p(dataVar, sample_data{(i)}.latitude);
        end
        dataVar(~iGood) = NaN;
        
        %add to the plot
        hLineVar(i + 1) = line(xLine, ...
            dataVar, ...
            'Color', cMap(i, :), ...
            'LineStyle', '-','Parent',hAxPress);
                        
        % set background to be grey
%         set(hAxPressDiff, 'Color', [0.75 0.75 0.75])
    end


end   
        
if isPlottable
    if saveToFile
        % ensure the printed version is the same whatever the screen used.
        set(hFigPress, 'PaperPositionMode', 'manual');
        set(hFigPress, 'PaperType', 'A4', 'PaperOrientation', 'landscape', 'PaperUnits', 'normalized', 'PaperPosition', [0, 0, 1, 1]);
        
        % preserve the color scheme
        set(hFigPress, 'InvertHardcopy', 'off');
        
        fileName = strrep(fileName, '_PARAM_', ['_', varName, '_']); % IMOS_[sub-facility_code]_[site_code]_FV01_[deployment_code]_[PLOT-TYPE]_[PARAM]_C-[creation_date].png
        fileName = strrep(fileName, '_PLOT-TYPE_', '_LINE_');
        
        % use hardcopy as a trick to go faster than print.
        % opengl (hardware or software) should be supported by any platform and go at least just as
        % fast as zbuffer. With hardware accelaration supported, could even go a
        % lot faster.
        imwrite(hardcopy(hFigPress, '-dopengl'), fullfile(exportDir, fileName), 'png');
        close(hFigPress);
    end
end

end