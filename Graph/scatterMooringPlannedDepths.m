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

%Ask for the user to select the region they would like to use for
%comparison
%This could be done better, with more finesse - could allow zooming in
%before going straight to the time period selection. For now, this will do.
helpdlg('Select the time period for comparison using the mouse','Time Period Selection');


%plot depth information
monitorRec = get(0,'MonitorPosition');
xResolution = monitorRec(:, 3)-monitorRec(:, 1);
iBigMonitor = xResolution == max(xResolution);
if sum(iBigMonitor)==2, iBigMonitor(2) = false; end % in case exactly same monitors

title = [sample_data{1}.deployment_code ' mooring planned depth vs measured depth ' stringQC '''d good ' varTitle];

%extract the essential data and
%sort instruments by depth
lenSampleData = length(sample_data);
metaDepth = nan(lenSampleData, 1);
xMin = nan(lenSampleData, 1);
xMax = nan(lenSampleData, 1);
dataVar = nan(lenSampleData,800000);
timeVar = dataVar;
for i=1:lenSampleData
    %only look at indexes with pres_rel
    iVar = getVar(sample_data{i}.variables, 'PRES_REL');
    if iVar==0
        continue
    end

    data = sample_data{(i)}.variables{iVar}.data;
    iTime = getVar(sample_data{i}.dimensions, 'TIME');
    if isQC
        iVar = getVar(sample_data{i}.variables, 'DEPTH');
        data = sample_data{(i)}.variables{iVar}.data;
        %get time and var QC information
        timeFlags = sample_data{(i)}.dimensions{iTime}.flags;
        varFlags = sample_data{(i)}.variables{iVar}.flags;
        
        iGood = (timeFlags == 0 | timeFlags == 1 | timeFlags == 2) ...
            & (varFlags == 1 | varFlags == 2);
        
    else
        %calculate depth
        iVar = getVar(sample_data{i}.variables,'LATITUDE');
        if isempty(iVar)
            disp(['No latitude for instrument serial number ' ...
                sample_data{i}.instrument_serial_number ', unable to calculate DEPTH'])
            return
        end
        data = -gsw_z_from_p(data, sample_data{i}.variables{iVar}.data);
        
        iGood = true(size(data));
    end
    
    
    if all(~iGood) && isQC
        fprintf('%s\n', ['Warning : in ' sample_data{(i)}.toolbox_input_file ...
            ', there is not any ' varName ' data with good flags.']);
        continue;
    end
   
    %collect the time data too
    time = sample_data{i}.dimensions{iTime}.data;

    data = data(iGood);
    time = time(iGood);
    
    %save the data into a holding matrix so we don't have to loop over the
    %sample_data matrix again.
    dataVar(i,1:length(data)) = data;
    timeVar(i,1:length(time)) = time;
    
    if ~isempty(sample_data{i}.meta.depth)
        metaDepth(i) = sample_data{i}.meta.depth;
    elseif ~isempty(sample_data{i}.instrument_nominal_depth)
        metaDepth(i) = sample_data{i}.instrument_nominal_depth;
    else
        metaDepth(i) = NaN;
    end
    
    xMin(i) = min(time);
    xMax(i) = max(time);
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
end
%only look at indexes with pres_rel
[metaDepth, iSort] = sort(metaDepth);
dataVar = dataVar(iSort,:);
timeVar = timeVar(iSort,:);
sample_data = sample_data(iSort);
instrumentDesc = instrumentDesc(iSort);
%delete non-pressure instrument information
ibad = nansum(dataVar,2)==0;
metaDepth(ibad) = [];
sample_data(ibad) = [];
dataVar(ibad,:) = [];
timeVar(ibad,:) = [];
instrumentDesc(ibad) = [];
xMin = min(xMin);
xMax = max(xMax);

%color map
cMap = jet(length(metaDepth));

%now plot all the calculated depths on one plot to choose region for comparison:
%plot
fileName = genIMOSFileName(sample_data{1}, 'png');
visible = 'on';
if saveToFile, visible = 'off'; end
hFigPress = figure(...
    'Name', title, ...
    'NumberTitle','off', ...
    'Visible', visible, ...
    'OuterPosition', [0, 0, monitorRec(iBigMonitor, 3), monitorRec(iBigMonitor, 4)]);

%depth plot for selecting region to compare depth to planned depth
hAxPress = subplot(2,1,1,'Parent', hFigPress);
set(hAxPress, 'YDir', 'reverse')
set(get(hAxPress, 'XLabel'), 'String', 'Time');
set(get(hAxPress, 'YLabel'), 'String', ['DEPTH (' varUnit ')'], 'Interpreter', 'none');
set(get(hAxPress, 'Title'), 'String', 'Depth', 'Interpreter', 'none');
set(hAxPress, 'XTick', (xMin:(xMax-xMin)/4:xMax));
set(hAxPress, 'XLim', [xMin, xMax]);
hold(hAxPress, 'on');

%now plot the data:Have to do it one at a time to get the colors right..
for i = 1:length(metaDepth)
    hLineVar(i) = line(timeVar(i,:), ...
        dataVar(i,:), ...
        'Color',cMap(i,:),...
        'LineStyle', '-',...
        'Parent',hAxPress);
end
isPlottable = true;

% set background to be grey
%         set(hAxPressDiff, 'Color', [0.75 0.75 0.75])

% Let's redefine properties after pcolor to make sure grid lines appear
% above color data and XTick and XTickLabel haven't changed
set(hAxPress, ...
    'XTick',        (xMin:(xMax-xMin)/4:xMax), ...
    'XGrid',        'on', ...
    'YGrid',        'on', ...
    'Layer',        'top');

if isPlottable    
    datetick(hAxPress, 'x', 'dd-mm-yy HH:MM:SS', 'keepticks');
    
    hLegend = legend(hAxPress, ...
        hLineVar,       instrumentDesc, ...
        'Interpreter',  'none', ...
        'Location',     'Best');
    
    set(hLegend,'Fontsize',10)
end

%select the area to use for comparison
[x,y] = select_points;

%Actual depth minus planned depth
hAxDepthDiff = subplot(2,1,2,'Parent', hFigPress);
set(get(hAxDepthDiff, 'XLabel'), 'String', 'Planned Depth (m)');
set(get(hAxDepthDiff, 'YLabel'), 'String', ['Actual Depth - Planned Depth (' varUnit ')'], 'Interpreter', 'none');
set(get(hAxDepthDiff, 'Title'), 'String', ...
    ['Differences from planned depth for ' sample_data{1}.meta.site_name] , 'Interpreter', 'none');
hold(hAxDepthDiff, 'on');
grid(hAxDepthDiff, 'on');


%now plot the difference from planned depth data:
iGood = timeVar >= x(1) & timeVar <= x(2);
dataVar(~iGood) = NaN;
minDep = min(dataVar,[],2);

hLineVar2 = line(metaDepth, ...
    minDep - metaDepth, ...
    'Color','k',...
    'LineStyle', 'None',...
    'Marker','o',...
    'Marker','.',...
    'MarkerSize',12,...
    'Parent',hAxDepthDiff);

text(metaDepth + 1, (minDep - metaDepth), instrumentDesc, ...
    'Parent', hAxDepthDiff)
        
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