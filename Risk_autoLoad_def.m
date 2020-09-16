%%%Carga de datos de entrada
tic
current_path = pwd; %%%aqui me ubico en la dirección actual
DeltaSOC_all = [];   %%%se acumulan los DeltaSOC

%%%%defino en que carpetas voy a escrutar los datos que tienen
folders =["1_min" , "2_median", "3_mean", "4_max"];
%%%ejecuto un lazo for desde 1 y hasta la última carpeta para cargar
%%%%y analizar TODOS los experimento ejecutados
for i=1:length (folders)
    %%%%aquí se captura el nombre de la carpeta (y el "i" es para que
    %%%%vaya avanzando uno a la vez en este caso son 4 carpetas
    Folder = strcat(current_path, '\', folders(i));
    
    %%%%ya que cada carpeta matriz contiene 64 subcarpetas se ejecuta este
    %%%%for de 1:64
    for j=1:64
        %%%%aqui se crea otro camino entre cada subcarpeta que se va
        %%%%aumentando (j) 1 a 1 hasta completar los 64
        path = strcat(Folder,'\CMkb1_',num2str(j));
        %%%% la funcion current directory (cd) para que siga el path
        cd(path);
        %%%%se cargan los datos de estado de la batería
        load soc
        %%%se hace el conteo de acumulación y se suma en cada iteración
        [i j];
        
        %%%%se miden los valores de la carga de las baterias
        soc_i=[soc.time([1]), soc.data([1])];
        soc_fin=[soc.time([end]), soc.data([end])];
        % %         soc_i=max(soc);
        % %         soc_fin=min(soc);
        %%%% se mide la diferencia al termino del experimento
        DeltaSOC=(soc_fin(2)-soc_i(2));
        % %           DeltaSOC=(soc_i(2)-soc_fin(2));
        DeltaSOC_all=[DeltaSOC; DeltaSOC_all]; %#ok<AGROW>
        
        cd(current_path);
        
    end
end

N_Elementos=8;  %%%%los elementos de los vectores
% %%%Distribucion de Placas en un incremento de 50 en 50 % partiendo de un
% 50% hasta llegar a un 400 % del valor base
PV_400=1105*4.*ones(N_Elementos,1);
PV350=1105*3.5.*ones(N_Elementos,1);
PV300=1105*3.*ones(N_Elementos,1);
PV250=1105*2.5.*ones(N_Elementos,1);
PV200=1105*2.*ones(N_Elementos,1);
PV150=1105*1.5.*ones(N_Elementos,1);
PV100=1105*1.*ones(N_Elementos,1);
PV50=1105*0.5.*ones(N_Elementos,1);
%
% %%% con esta función "flip" lo que hago es ordenar los elementos de manera
% %%% Ascendente
PV1 =flip([PV_400; PV350; PV300; PV250; PV200; PV150; PV100; PV50]);
% % %
% % % %%%%Distribucion de las baterias desde
% %%%al igual que con los paneles lo hago con la baterias de 50% a 400%
Bateries_100=186;

B10=Bateries_100*4;
B9=Bateries_100*3.5;
B8=Bateries_100*3;
B7=Bateries_100*2.5;
B6=Bateries_100*2;
B5=Bateries_100*1.5;
B4=Bateries_100*1;
B3=Bateries_100*0.5;
%
% %%%al igual que con los paneles las baterías se ordenan de manera tal que
% %%%sean Ascendente el orden
u =flip([B10; B9; B8; B7; B6; B5; B4; B3]);
BATT1=repmat(u,N_Elementos,1); %%%%%para convertir el VECTOR "u"
%%%en un arreglo de 64 elementos para permutarlo con las otras 2
%%%variables (irr y PV)
% % % %
% % % %
% % % % % a=delta_soc
% % %
FC=(10000/3600);   %%%%FACTOR de converción de Decenas de Kilo Julios a W·h
% %%%Se llama a la data historica de Excel
% FC=(10/3600);
I=xlsread('Radiacin1082_curada.xlsx',1,'v2:v11522')*FC;
% %
% % % % % % I=I1(10000:end);  %%%%%eSTO SE HACE PARA PROBAR LA DATA DESDE UN
% % % INICIO DISTINTO AL INICIO DE LA MISMA
% % % % %%%%%Irradiación kJ/m2 proviene de decenas KiloJulios por dias
% % % % %               %%%Lo MULTIPLICO por 2.777778 para convertirlas en Watios
% % %
% % % %%%Limpieza y ordenamiento de la data
% % % idx=isnan(I); %%%%Se verifican los valores NAN (que no el corte del suministro)
% % % I(idx)=mode(I);   %%%% se cambia por nan para que no altere el resultado
% % %
format bank   %%%%para que las fechas salgan como un número entero
%
F=xlsread('Radiacin1082_curada.xlsx',1,'B2:D11522');
IRR=[F, I]; %%%%Cojo los datos que me interesan fecha y lo vinculo con irradiacion
% % %
% % % %%%%%%se hallan los indices de los valores analizados
[cmin, indice_min]=min(IRR(:,4));   %%%%aquí se halla el valor minimo y su indice (ubicación)
min_dia=IRR(indice_min, 1:4);       %%%%Aquí el indice se usa para extraer los elemento del 1 al 3 que se vinculan con este valor
disp(min_dia)

cmedian=median(IRR(:,4),('omitnan'));   %%%%aquí se halla el valor minimo y su indice (ubicación)
[row_median, col_median]=find(IRR==(cmedian),1);
median_dia=IRR(col_median, 1:4);       %%%%Aquí el indice se usa para extraer los elemento del 1 al 3 que se vinculan con este valor
disp(median_dia)

cmean=mean(IRR(:,4), ('omitnan'));   %%%%aquí se halla el valor minimo y su indice (ubicación)
[row,col]=find(IRR>=(cmean-1)&IRR<=(cmean+1), 1);  %%%%Aquí encierro un valor que se corresponde al medio
mean_dia=IRR(row, 1:4);       %%%%Aquí el indice se usa para extraer los elemento del 1 al 3 que se vinculan con este valor
disp(mean_dia)

[cmax, indice_max]=max(IRR(:,4));   %%%%aquí se halla el valor minimo y su indice (ubicación)
max_dia=IRR(indice_max, 1:4);       %%%%Aquí el indice se usa para extraer los elemento del 1 al 3 que se vinculan con este valor
disp(max_dia)
% % %
% % % % %%%Variable de entrada de irradiación organizada por los grupos
% % % % anteriormente definidos
% % %
irr1 = ones(64, 1);
I_1=irr1*cmin;
I_3=irr1*cmedian;
I_4=irr1*cmean;
I_5=irr1*cmax;
% % %
% % s=flip(reshape(DeltaSOC_all,[64,1]));
s=flip(reshape(DeltaSOC_all,[64,4]));   %%%%%Aqui genero una matriz de ROW by COL a
%                                        %%%%partir de DeltaSOC_all
%                                        %%%%%luego le aplico un flip para organizarlo en
%                                        %%%%%el mismo orden que se ejecutaron los
%                                        %%%experimentos
% %%%%%%%%Auqui saco cada uno de los vectores columnas para construir el
delta_soc1=s(:,4);  %%%%%SOC_min
delta_soc2=s(:,3);  %%%%%SOC_median
delta_soc4=s(:,2);  %%%%%SOC_mean
delta_soc5=s(:,1);  %%%%%SOC_max

% % % % %%%%%%%Aqui ya estan las variables de entrada
Variable_in=[I_1, delta_soc1, I_3, delta_soc2, I_4, delta_soc4, I_5, delta_soc5, PV1, BATT1];
% Variable_in=[I_1, delta_soc1, PV1, BATT1];
% % %
save('Variable_in.mat')   %%%%%Se salva la data en una array de MATLAB
%
% % % % %%%%La funcion de Riesgo
GRA = ones(8,8);
BATT = flip(unique(BATT1));
PV = flip(unique(PV1));
% % %

% for b=1:8
%     for p=1:8
%         [d,SOC_r,DSOC_r] = Riesgo(Variable_in,BATT(b),PV(p),I);
%         GRA(b,p) = d;
%         %%%%Un par de lineas extra para corregir el error que se produce
%         %%%%por el tamaño de SOC_r, que cuando se aplica el numel(SOC_r) se
%         %%%%excede en "1" valor. Esto hacia que cascara la generación de
%         %%%%archivos ".xlsx" de linea 163
%         length_DSOC_R = length(DSOC_r);
%         SOC_r = SOC_r(1:length_DSOC_R);
%         xlswrite(['SOC' '_(b)' num2str(b) '_' num2str(p), '.xls'],[SOC_r,DSOC_r,I]);
%         plot(I)
%         plot(SOC_r)
% %         plot(DSOC_r(1:d));
%         title(['DSOC: BATT=' num2str(b) ' PV=' num2str(p) ' d=' num2str(d)]);
%         print('graficas.ps','-dps','-append');
%     end
% end
% 
% xlswrite('days.xlsx',GRA)
% xlswrite('Variables_in.xlsx',Variable_in)
% 
% toc