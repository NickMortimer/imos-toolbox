function lineMooringPresDiffs(sample_data, sampleMenu, isQC, saveToFile, exportDir)
%LINEMOORINGPRESDIFFS Opens a new window where the PRES_REL
% variable collected by the selected intrument is plotted, and the difference
% in pressure from this instrument to adjacent instruments is plotted.
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
error(nargchk(5,5,nargin));

if ~iscell(sample_data),    error('sample_data must be a cell array');  end
if ~islogical(isQC),        error('isQC must be a logical');            end
if ~islogical(saveToFile),  error('saveToFile must be a logical');      end
if ~ischar(exportDir),      error('exportDir must be a string');        end

varTitle = imosParameters('PRES_REL', 'long_name');
varUnit = imosParameters('PRES_REL', 'uom');

stringQC = 'non QC';
if isQC, stringQC = 'QC'; end

%plot depth information
monitorRec = get(0,'MonitorPosition');
xResolution = monitorRec(:, 3)-monitorRec(:, 1);
iBigMonitor = xResolution == max(xResolution);
if sum(iBigMonitor)==2, iBigMonitor(2) = false; end % in case exactly same monitors

title = [sample_data{1}.deployment_code ' mooring pressure differences ' stringQC '''d good ' varTitle];

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

%first find the instrument of interest:
isam = find(iSort == sampleMenu.Value);

instrumentDesc = cell(length(metaDepth) + 1, 1);
hLineVar = nan(length(metaDepth) + 1, 1);
hLineVar2 = hLineVar;

isPlottable = false;

% instrument description
if ~isempty(strtrim(sample_data{isam}.instrument))
    instrumentDesc{1} = sample_data{isam}.instrument;
elseif ~isempty(sample_data{isam}.toolbox_input_file)
    [~, instrumentDesc{1}] = fileparts(sample_data{isam}.toolbox_input_file);
end

instrumentSN = '';
if ~isempty(strtrim(sample_data{isam}.instrument_serial_number))
    instrumentSN = [' - ' sample_data{isam}.instrument_serial_number];
end

instrumentDesc{1} = [strrep(instrumentDesc{1}, '_', ' ') ' (' num2str(metaDepth(isam)) 'm' instrumentSN ')'];

%plot
fileName = genIMOSFileName(sample_data{isam}, 'png');
visible = 'on';
if saveToFile, visible = 'off'; end
hFigPressDiff = figure(...
    'Name', title, ...
    'NumberTitle','off', ...
    'Visible', visible, ...
    'OuterPosition', [0, 0, monitorRec(iBigMonitor, 3), monitorRec(iBigMonitor, 4)]);

%pressure plot
hAxPress = subplot(2,1,1,'Parent', hFigPressDiff);
set(hAxPress, 'YDir', 'reverse')
set(get(hAxPress, 'XLabel'), 'String', 'Time');
set(get(hAxPress, 'YLabel'), 'String', ['PRES_REL (' varUnit ')'], 'Interpreter', 'none');
set(get(hAxPress, 'Title'), 'String', 'Pressure', 'Interpreter', 'none');
set(hAxPress, 'XTick', (xMin:(xMax-xMin)/4:xMax));
set(hAxPress, 'XLim', [xMin, xMax]);
hold(hAxPress, 'on');

%Pressure diff plot
hAxPressDiff = subplot(2,1,2,'Parent', hFigPressDiff);
set(get(hAxPressDiff, 'XLabel'), 'String', 'Time');
set(get(hAxPressDiff, 'YLabel'), 'String', ['PRES_REL (' varUnit ')'], 'Interpreter', 'none');
set(get(hAxPressDiff, 'Title'), 'String', ...
    ['Pressure Differences from ' instrumentDesc{1}] , 'Interpreter', 'none');
set(hAxPressDiff, 'XTick', (xMin:(xMax-xMin)/4:xMax));
set(hAxPressDiff, 'XLim', [xMin, xMax]);
hold(hAxPressDiff, 'on');

linkaxes([hAxPressDiff,hAxPress],'x')

%zero line
hLineVar(1) = line([xMin, xMax], [0, 0], 'Color', 'black');

%now plot the data of interest:
iTime = getVar(sample_data{isam}.dimensions, 'TIME');
iVar = getVar(sample_data{isam}.variables, 'PRES_REL');
iGood = true(size(sample_data{isam}.variables{iVar}.data));
samLine = sample_data{isam}.dimensions{iTime}.data;
samLine(~iGood) = NaN;

samVar = sample_data{isam}.variables{iVar}.data;
samVar(~iGood) = NaN;

hLineVar(1) = line(samLine, ...
    samVar, ...
    'Color', 'k', ...
    'LineStyle', '-',...
    'Parent',hAxPress);

%now get the adjacent instruments based on planned depth (up to 2 each side)
md = metaDepth(isam);
sample_data(isam) = [];
metaDepth(isam) = [];
[~,iothers] = sort(abs(md - metaDepth));
iothers = iothers(2:5);

%color map
cMap = hsv(length(iothers));

%now add the other data:
for i=1:length(iothers)
    % instrument description
    if ~isempty(strtrim(sample_data{iothers(i)}.instrument))
        instrumentDesc{i + 1} = sample_data{iothers(i)}.instrument;
    elseif ~isempty(sample_data{iothers(i)}.toolbox_input_file)
        [~, instrumentDesc{i + 1}] = fileparts(sample_data{iothers(i)}.toolbox_input_file);
    end
    
    instrumentSN = '';
    if ~isempty(strtrim(sample_data{iothers(i)}.instrument_serial_number))
        instrumentSN = [' - ' sample_data{iothers(i)}.instrument_serial_number];
    end
    
    instrumentDesc{i + 1} = [strrep(instrumentDesc{i + 1}, '_', ' ') ' (' num2str(metaDepth(iothers(i))) 'm' instrumentSN ')'];
        
    %look for time and relevant variable
    iTime = getVar(sample_data{iothers(i)}.dimensions, 'TIME');
    iVar = getVar(sample_data{iothers(i)}.variables, 'PRES_REL');    
    
    iGood = true(size(sample_data{iothers(i)}.variables{iVar}.data));
    
    if isQC
        %get time and var QC information
        timeFlags = sample_data{iothers(i)}.dimensions{iTime}.flags;
        varFlags = sample_data{iothers(i)}.variables{iVar}.flags;
        
        iGood = (timeFlags == 0 | timeFlags == 1 | timeFlags == 2) & (varFlags == 1 | varFlags == 2);
    end
    
    if all(~iGood) && isQC
        fprintf('%s\n', ['Warning : in ' sample_data{iothers(i)}.toolbox_input_file ...
            ', there is not any ' varName ' data with good flags.']);
        continue;
    else
        isPlottable = true;
        
        xLine = sample_data{iothers(i)}.dimensions{iTime}.data;
        xLine(~iGood) = NaN;
        
        dataVar = sample_data{iothers(i)}.variables{iVar}.data;
        dataVar(~iGood) = NaN;
        
        %add pressure to the pressure plot
        hLineVar(i + 1) = line(xLine, ...
            dataVar, ...
            'Color', cMap(i, :), ...
            'LineStyle', '-','Parent',hAxPress);
                
        %now put the data on the same timebase as the instrument of
        %interest
        newdat = match_timebase(samLine,xLine,dataVar);
        pdiff = samVar - newdat;
        
        hLineVar2(i + 1) = line(samLine, ...
            pdiff, ...
            'Color', cMap(i, :), ...
            'LineStyle', '-','Parent',hAxPressDiff);
        
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

set(hAxPressDiff, ...
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
        
    datetick(hAxPressDiff, 'x', 'dd-mm-yy HH:MM:SS', 'keepticks');
    datetick(hAxPress, 'x', 'dd-mm-yy HH:MM:SS', 'keepticks');
    
    hLegend = legend(hAxPress, ...
        hLineVar,       instrumentDesc, ...
        'Interpreter',  'none', ...
        'Location',     'SouthOutside');
    
    set(hLegend,'Fontsize',14)
    
%     % unfortunately we need to do this hack so that we have consistency with
%     % the case above
%     posAx = get(hAxPressDiff, 'Position');
%     set(hAxPressDiff, 'Position', posAx);
    
    %     set(hLegend, 'Box', 'off', 'Color', 'none');
    
    if saveToFile
        % ensure the printed version is the same whatever the screen used.
        set(hFigPressDiff, 'PaperPositionMode', 'manual');
        set(hFigPressDiff, 'PaperType', 'A4', 'PaperOrientation', 'landscape', 'PaperUnits', 'normalized', 'PaperPosition', [0, 0, 1, 1]);
        
        % preserve the color scheme
        set(hFigPressDiff, 'InvertHardcopy', 'off');
        
        fileName = strrep(fileName, '_PARAM_', ['_', varName, '_']); % IMOS_[sub-facility_code]_[site_code]_FV01_[deployment_code]_[PLOT-TYPE]_[PARAM]_C-[creation_date].png
        fileName = strrep(fileName, '_PLOT-TYPE_', '_LINE_');
        
        % use hardcopy as a trick to go faster than print.
        % opengl (hardware or software) should be supported by any platform and go at least just as
        % fast as zbuffer. With hardware accelaration supported, could even go a
        % lot faster.
        imwrite(hardcopy(hFigPressDiff, '-dopengl'), fullfile(exportDir, fileName), 'png');
        close(hFigPressDiff);
    end
end

end